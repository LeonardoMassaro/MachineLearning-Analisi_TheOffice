attach(dati_episodi2)

dati_episodi1 <- scale(dati_episodi2)
dati_episodi2 <- as.data.frame(dati_episodi1)
######################regressione con un solo regressore##################
mod_scene_distinte <- lm(Rating~n_scene_distinte, data=dati_episodi2)
plot(mod_scene_distinte)
summary(mod_scene_distinte)


mod_scene_distinte <- lm(Rating~n_scene_distinte, data=dati_episodi2)
plot(mod_scene_distinte)
summary(mod_scene_distinte)


mod_HHI_battute <- lm(Rating~HHI_battute,data = dati_episodi2)
plot(mod_HHI_battute)
summary(mod_HHI_battute)


mod_HHI_scene <- lm(Rating~HHI_scene,data = dati_episodi2)
plot(mod_HHI_scene)
summary(mod_HHI_scene)


mod_dur_media_scene<-lm(Rating~dur_media_scene,data = dati_episodi2)
plot(mod_dur_media_scene)
summary(mod_dur_media_scene)

mod_n_cambi_scena <- lm(Rating~n_cambi_scena, data=dati_episodi2)
plot(mod_n_cambi_scena)
summary(mod_n_cambi_scena)


#######################regressioni con coppie di regressori##############

mod_cambi_scena_dur_media <-lm(Rating~n_cambi_scena+dur_media_scene,data = dati_episodi2)
plot(mod_cambi_scena_dur_media)
summary(mod_cambi_scena_dur_media)



mod_cambi_scena_HHI_scene <-lm(Rating~n_cambi_scena+HHI_scene,data = dati_episodi2)
plot(mod_cambi_scena_HHI_scene)
summary(mod_cambi_scena_HHI_scene)



mod_cambi_scena_HHI_battute <-lm(Rating~n_cambi_scena+HHI_battute,data = dati_episodi2)
plot(mod_cambi_scena_HHI_battute)
summary(mod_cambi_scena_HHI_battute)



mod_cambi_scena_scene_distinte <-lm(Rating~n_cambi_scena+n_scene_distinte,data = dati_episodi2)
plot(mod_cambi_scena_scene_distinte)
summary(mod_cambi_scena_scene_distinte)



mod_HHI_battute_HHI_scene <-lm(Rating~HHI_battute+HHI_scene,data = dati_episodi2)
plot(mod_HHI_battute_HHI_scene)
summary(mod_HHI_battute_HHI_scene)




mod_HHI_battute_dur_scene <-lm(Rating~HHI_battute+dur_media_scene,data = dati_episodi2)
plot(mod_HHI_battute_dur_scene)
summary(mod_HHI_battute_dur_scene)




mod_HHI_battute_scene_distinte <-lm(Rating~HHI_battute+n_scene_distinte,data = dati_episodi2)
plot(mod_HHI_battute_scene_distinte)
summary(mod_HHI_battute_scene_distinte)



mod_scene_distinte_HHI_scene <-lm(Rating~n_scene_distinte+HHI_scene,data = dati_episodi2)
plot(mod_scene_distinte_HHI_scene)
summary(mod_scene_distinte_HHI_scene)



mod_scene_distinte_dur_scene <-lm(Rating~n_scene_distinte+dur_media_scene,data = dati_episodi2)
plot(mod_scene_distinte_dur_scene)
summary(mod_scene_distinte_dur_scene)




mod_HHI_scene_durata_scene <-lm(Rating~HHI_scene+dur_media_scene,data = dati_episodi2)
plot(mod_HHI_scene_durata_scene)
summary(mod_HHI_scene_durata_scene)



#######################regressioni con 3 di regressori##############################


r123<-lm(Rating~n_cambi_scena+dur_media_scene+HHI_battute,data = dati_episodi2)
plot(r123)
summary(r123)




r124 <-lm(Rating~n_cambi_scena+dur_media_scene+HHI_scene,data = dati_episodi2)
plot(r124)
summary(r124)




r125 <-lm(Rating~n_cambi_scena+dur_media_scene+n_scene_distinte,data = dati_episodi2)
plot(r125)
summary(r125)




r134 <-lm(Rating~n_cambi_scena+HHI_battute+HHI_scene,data = dati_episodi2)
plot(r134)
summary(r134)



r135 <-lm(Rating~n_cambi_scena+HHI_battute+n_scene_distinte,data = dati_episodi2)
plot(r135)
summary(r135)




r145 <-lm(Rating~n_cambi_scena+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r145)
summary(r145)




r234 <-lm(Rating~dur_media_scene+HHI_battute+HHI_scene,data = dati_episodi2)
plot(r234)
summary(r234)




r235 <-lm(Rating~dur_media_scene+HHI_battute+n_scene_distinte,data = dati_episodi2)
plot(r235)
summary(r235)



r245 <-lm(Rating~dur_media_scene+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r245)
summary(r245)



#HHI_battute+n_scene_distinte+HHI_scene
r345<-lm(Rating~HHI_battute+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r345)
summary(r345)



########################### 4 regressori ##########################

r1234 <-lm(Rating~n_cambi_scena+dur_media_scene+HHI_battute+HHI_scene,data = dati_episodi2)
plot(r1234)
summary(r1234)

r1235 <-lm(Rating~n_cambi_scena+dur_media_scene+HHI_battute+n_scene_distinte,data = dati_episodi2)
plot(r1235)
summary(r1235)

r1245 <-lm(Rating~n_cambi_scena+dur_media_scene+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r1245)
summary(r1245)

r1345 <-lm(Rating~n_cambi_scena+HHI_battute+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r1345)
summary(r1345)

r2345<-lm(Rating~dur_media_scene+HHI_battute+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r2345)
summary(r2345)

########################### 5 regressori ##########################

r12345 <-lm(Rating~n_cambi_scena+dur_media_scene+HHI_battute+HHI_scene+n_scene_distinte,data = dati_episodi2)
plot(r12345)
summary(r12345)
