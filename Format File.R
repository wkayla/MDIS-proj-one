
library(tidyverse)
load("A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/SpinalRawData.Rdata")

mdis=read.csv("C:/Users/wkayla/OneDrive - The University of Colorado Denver/Rzasalynn/Data/MDIS FDA EPC.csv",header=T)
allergies=read.csv("C:/Users/wkayla/OneDrive - The University of Colorado Denver/Rzasalynn/Data/Allergies.csv",header=T)


filenames <- list.files("A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)

names(ldf)<-gsub("A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/","",filenames)

ldf=lapply(ldf,function(x){x$person_id=as.character(x$person_id)
                       return(x)})

list2env(ldf ,.GlobalEnv)

Table7_inpatient_meds_20190913.csv=read.csv("A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/Table7_inpatient_meds_20190913.csv",header=T)

Spine_procedures.csv=Spine_procedures.csv%>%
                        group_by(person_id)%>%
                        arrange(days_from_dob_procstart)%>%
                        mutate(spine_proc_num=row_number(),
                               spine_proc="Yes")%>%
                        arrange(spine_proc_num)%>%
                        mutate(first_spinal_time=days_from_dob_procstart[1])

spine_surg=Spine_procedures.csv%>%
            mutate(spine_time_in_pacu=time_in_pacu,
                   spine_time_out_pacu=time_out_pacu,
                   spine_discharge_time=discharge_time,
                   spine_LengthOfStayInDays=LengthOfStayInDays)%>%
            select(-time_in_pacu,-time_out_pacu,-discharge_time,-LengthOfStayInDays)%>%
            right_join(Table6_surg_procedures.csv)



time=Spine_procedures.csv%>%ungroup()%>%select(person_id,first_spinal_time)
# 


#merge procedure datasets. Filter down to only person IDs in spinal visits and delete duplicate encounters.
procedures=full_join(Table5_procedures.csv,spine_surg,by=c("person_id","encounter_id"))%>%
              filter(person_id%in%Spine_procedures.csv$person_id)%>%
              distinct(person_id,encounter_id,.keep_all = T)%>%
              mutate(order_name=order_name.x ,
                     DepartmentName=DepartmentName.x,
                     days_from_dob_procstart=days_from_dob_procstart.x,
                     proc_order_name=order_name.y,
                     proc_DepartmentName=DepartmentName.y,
                     proc_days_from_dob_procstart=days_from_dob_procstart.y)%>%
              select(-order_name.x,-DepartmentName.x,-days_from_dob_procstart.x, 
                     -order_name.y,-DepartmentName.y,-days_from_dob_procstart.y)


procedures$person_id=as.character(procedures$person_id)
Encounters$encounter_id=as.character(Encounters$encounter_id)
procedures$encounter_id=as.character(procedures$encounter_id)
#merge primary spinal procedure with all procedures

spinal_procedures=Encounters%>%
            mutate(encounter_DepartmentName=DepartmentName,
                   encounter_LocationName=LocationName,
                   encounter_days_from_dob_toencounter=days_from_dob_toencounter)%>%
            select(-DepartmentName,-LocationName,-days_from_dob_toencounter)%>%
            full_join(procedures,by=c("person_id","encounter_id"))


#time between procedure and first spinal procedure. filter out procedures fall outside 30 days for hospital and 90 days for surgery.
time$person_id=as.character(time$person_id)
Table1_patient.csv$person_id=as.character(Table1_patient.csv$person_id)
spinal_procedures_sub=spinal_procedures%>%
                    group_by(person_id)%>%
                    select(-first_spinal_time)%>%
                    right_join(time)%>%
                    mutate(time_since_proc=as.numeric(as.character(days_from_dob_procstart))-as.numeric(as.character(first_spinal_time)),
                           time_since_encounter=as.numeric(as.character(proc_days_from_dob_procstart))-as.numeric(as.character(first_spinal_time)))%>%
                    left_join(Table1_patient.csv)

#create infection and respiratory variables

diagnoses=Table4_dx.csv%>%
  filter(person_id%in%spinal_procedures_sub$person_id)%>%
  mutate(infection=ifelse(code%in%c("T81.4XXA","682.9","R78.81","A41.9","A41.51","M86.9",
                                    "879.9","D72.829","R50.9","M46.26","R65.21","N39.0","M46.42","M46.44",
                                    "M46.40", "M46.45","M46.47","G06.1","M86.10","995.91","J20.9","J18.9","T81.12XA"),"Yes","No"),
         respiratory_infection=ifelse(code%in%c("J96.01","J96.00", "J18.1", "J96.02", "J95.821"),"Yes","No"))%>%
  distinct(person_id,encounter_id,infection,respiratory_infection,.keep_all = T)%>%
  mutate_at(vars(person_id,encounter_id),as.character)


spinal_procedures_diagnoses=left_join(spinal_procedures_sub,diagnoses)

spinal_procedures_meds=Table7_inpatient_meds_20190913.csv%>%
                            mutate_at(vars(person_id,encounter_id),as.character)%>%
                            right_join(spinal_procedures_diagnoses)%>%
                            mutate(med_name=Name)%>%
                            select(-Name)%>%
                            filter((time_since_encounter>=0) |time_since_proc>=0)

#allergy data

mdis$Active.Moiety.Name=trimws(mdis$Active.Moiety.Name,which = "right")
mdis$FDA.Established.Pharmacologic.Class..EPC..Text.Phrase=trimws(mdis$FDA.Established.Pharmacologic.Class..EPC..Text.Phrase,which = "right")

allergies=allergies%>%
  mutate(Name=tolower(allergies$Name))%>%
  filter(`Drug.`=="Y" | `Drug.`=="")%>%
  left_join(mdis,by=c("Name"="Active.Moiety.Name"))%>%
  mutate(classified=ifelse(is.na(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase),"No","Yes"),
         FDA.Established.Pharmacologic.Class..EPC..Text.Phrase=
           ifelse(is.na(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase),
                  as.character(Name),
                  FDA.Established.Pharmacologic.Class..EPC..Text.Phrase))

allergies=allergies%>%
  distinct(Name,`FDA.Established.Pharmacologic.Class..EPC..Text.Phrase`,.keep_all=T)

allergy_class=Table2_allergy.csv%>%
  mutate(Name=tolower(Name))%>%
  left_join(allergies,by=c("Name"="Name"))%>%
  mutate(classified=ifelse(is.na(classified),"No","Yes"),
         FDA.Established.Pharmacologic.Class..EPC..Text.Phrase=
           ifelse(is.na(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase),
                  as.character(Name),
                  FDA.Established.Pharmacologic.Class..EPC..Text.Phrase))

spinal_allergy=allergy_class %>% 
  mutate(person_id=as.character(person_id))%>%
  right_join(spinal_procedures_meds)%>%
  group_by(person_id)%>%
  mutate(allergy=ifelse(is.na(Name),"No","Yes"),
         Name=ifelse(is.na(Name),"None",as.character(Name)),
         classes=length(unique(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase)),
         num_drugs=length(unique(Name)),
         `Drug.`=ifelse(is.na(Drug.),"Not in allergies",as.character(`Drug.`)))%>%
  mutate(spine_proc=ifelse(is.na(spine_proc),"No","Yes"))




spinal_allergy_sub=spinal_allergy%>%
                      filter(encounter_type%in%c("Hospital Encounter","Anesthesia",
                                                 "Surgery","Anesthesia Event","Hospital","Procedure Visit"))%>%
                      mutate(Name=ifelse(is.na(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase),
                                         ifelse(Name=="None","None","Not Classified"),as.character(FDA.Established.Pharmacologic.Class..EPC..Text.Phrase)))

save(spinal_allergy_sub,file="A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/analytic_data.Rdata")

unique_icd=Table4_dx.csv%>%filter(person_id%in%spinal_procedures_sub$person_id)%>%distinct(code,code_description)

write.csv(unique_icd,"A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/ICDs_in_data.csv",row.names=F)

location=spinal_allergy%>%distinct(encounter_LocationName)
write.csv(location,"A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/location.csv",row.names=F)


write.csv(not_classified,"A:/Shared/DataLibrary/CA_Anesthesiology/AP0008RzasaLynn/MDIS Not Classified.csv",row.names=F)
