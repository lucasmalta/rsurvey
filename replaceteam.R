
replaceteam <- function(mydata){

  levels(mydata$Please.state.your.Agile.team.s.name.) <- tolower(levels(mydata$Please.state.your.Agile.team.s.name.))
  levels(mydata$Please.state.your.Agile.team.s.name.)[levels(mydata$Please.state.your.Agile.team.s.name.)=="shared services - hurtz, i-team, cobra, backbone"] <- "shared"

  # SHARED
  shared = grep('shared', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)

  #ADAS
  azure_adas = grep('azure', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  backbone_adas = grep('backbone', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  cobra_adas = grep('cobra', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  surfers_adas = grep('surfers', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  concept_adas = grep('concept', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  florida_adas = grep('florida', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  dragon_adas = grep('dragon', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  turtles_adas = grep('turtles', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  fiftyone_adas = grep('51', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  california_adas = grep('california', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  calong_adas = grep('ca-long|ca long|calong', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  legacy_adas = grep('legacy', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  hurtz_adas = grep('hurtz', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  dmon_adas = grep('d-mon', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  iteam_adas = grep('i-team', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  
  # Fix dragon/azure dragon
  dragon_adas = dragon_adas[-match(azure_adas,dragon_adas)]
  
  # Grouping ADAS
  driver_assist_adas = c(azure_adas, florida_adas)
  low_speed_manv_adas = c(dragon_adas, concept_adas, surfers_adas, turtles_adas)
  domain_hwsw_adas = c(backbone_adas, legacy_adas)
  radar_sensor_adas = c(cobra_adas, hurtz_adas)
  vision_sensor_adas = c(dmon_adas, iteam_adas)
  collision_avd_adas = c(california_adas, calong_adas, fiftyone_adas)
  

  #AD
  drive1_ad = grep('drive 1|drive1', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  drive2_ad = grep('drive 2', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  drive3_ad = grep('drive 3|lund|drive #3', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  scenario_ad = grep('scenario', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  maps_ad = grep('maps|map&cloud', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  fusion_ad = grep('fusion', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  adpm_ad = grep('adpm', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  surround_ad = grep('surround|surroun', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  radar_ad = grep('radar', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  vision_ad = grep('vision', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  lidar_ad = grep('lidar|beam', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  adcore_ad = grep('ad core', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  datadev_ad = grep('data development system', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)

  # Grouping AD
  sensing_ad = c(radar_ad, lidar_ad, surround_ad, vision_ad)
  compute_platform_ad = c(adpm_ad, adcore_ad, datadev_ad)
  env_perception_ad = c(maps_ad, fusion_ad)
  hwy_pilot_ad = c(drive1_ad, scenario_ad)
  low_speed_ad = c(drive3_ad)
  
  #VMC
  ravens_vmc = grep('raven', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  statesmen_vmc = grep('vehicle state|statesmen', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)

  # Grouping VMC
  longlat_vmc = grep('long/lat1|lat/long|long/lat2|long/lat', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  stab_vmc = grep('stability', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  brk_vmc = grep('brake', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  vehicle_state_vmc = c(statesmen_vmc, ravens_vmc)
  exteral_vmc = grep('external', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  adhoc_vmc = grep('adhoc', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  syssafety_vmc = grep('system safety', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  

  # PROTECTIVE
  heart_prosaf = grep('heart', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  sunshine_prosaf = grep('sunshine', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  luxury_prosaf = grep('luxury', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  roadrunner_prosaf = grep('roadrunner', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)
  automad_prosaf = grep('autonomad', levels(mydata$Please.state.your.Agile.team.s.name.), value = TRUE)

  groups = c(
    'low_speed_manv_adas',
    'domain_hwsw_adas',
    'radar_sensor_adas',
    'vision_sensor_adas',
    'collision_avd_adas',
    'driver_assist_adas',
    'sensing_ad',
    'compute_platform_ad',
    'env_perception_ad',
    'hwy_pilot_ad',
    'low_speed_ad',
    'longlat_vmc',
    'stab_vmc',
    'brk_vmc',
    'vehicle_state_vmc',
    'exteral_vmc',
    'adhoc_vmc',
    'syssafety_vmc',
    'heart_prosaf',
    'sunshine_prosaf',
    'luxury_prosaf',
    'roadrunner_prosaf',
    'automad_prosaf'
  )

  teams=c(
  'azure_adas',
  'backbone_adas',
  'cobra_adas',
  'surfers_adas',
  'concept_adas',
  'florida_adas',
  'dragon_adas',
  'turtles_adas',
  'fiftyone_adas',
  'california_adas',
  'calong_adas',
  'legacy_adas',
  'hurtz_adas',
  'dmon_adas',
  'iteam_adas',
  'drive1_ad',
  'drive2_ad',
  'drive3_ad',
  'scenario_ad',
  'maps_ad',
  'fusion_ad',
  'adpm_ad',
  'surround_ad',
  'radar_ad',
  'vision_ad',
  'lidar_ad',
  'adcore_ad',
  'datadev_ad',
  'stab_vmc',
  'brk_vmc',
  'longlat_vmc',
  'ravens_vmc',
  'statesmen_vmc',
  'adhoc_vmc',
  'syssafety_vmc',
  'exteral_vmc',
  'shared',
  'heart_prosaf',
  'sunshine_prosaf',
  'luxury_prosaf',
  'roadrunner_prosaf',
  'automad_prosaf')
  

  for (i in groups){
    for (j in eval(parse(text=i))){
      levels(mydata$Please.state.your.Agile.team.s.name.)[levels(mydata$Please.state.your.Agile.team.s.name.)== j ] <- i
    }
  }

#  for (i in teams){
#    for (j in eval(parse(text=i))){
#      levels(mydata$Please.state.your.Agile.team.s.name.)[levels(mydata$Please.state.your.Agile.team.s.name.)== j ] <- i
#    }
#  }


  w = levels(mydata$Please.state.your.Agile.team.s.name.)
  w[!(levels(mydata$Please.state.your.Agile.team.s.name.) %in% groups)] <- 'Other'
  levels(mydata$Please.state.your.Agile.team.s.name.) <- w
  
  return(mydata)

}

