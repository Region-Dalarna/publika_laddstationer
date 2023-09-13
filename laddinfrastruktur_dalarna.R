#Laddinfrastruktur i Dalarna
##publika laddstationer
###skriptet hämtar data på laddstationer, vägar och kommungränser samt skapar en interaktiv karta i Mapview

if (!require("pacman")) install.packages("pacman")
p_load(httr,
       jsonlite,
       mapview,
       tidyverse,
       tidyr,
       sf,
       tmap,
       dplyr,
       leaflet,
       ggpubr, 
       keyring,
       RPostgreSQL,
       RPostgres)

#Hämta punktlagret med publika laddstationer från Nobil
##
###Hämta din personliga api-nyckel från Nobil via Energimyndigheten 
####https://www.energimyndigheten.se/klimat--miljo/transporter/laddinfrastruktur/registrera-din-laddstation/utvecklare/

#Skapa variabler för webbadress, api-nyckel och diverse. 

# url <- "https://nobil.no/api/server/datadump.php?apikey="
# api_key <- "<din_api_nyckel>"            #ta bort < och > lägg till din apinyckel således med citattecken runt "din_api_nyckel"
div <- "&countrycode=SWE&fromdate=2012-06-02&format=json&file=false"

##Eller lägg API-information i en lösenordshanterare som t.ex keyring
url = key_list(service = "nobil")$username
api_key = key_get("nobil", key_list(service = "nobil")$username)

laddst_sv <- GET(paste0(url, api_key, div))  #paste0 sammanfogar variablerna url, api_key och div utan mellanrum
laddst_sv_resp <- fromJSON(content(laddst_sv, as = "text"), flatten = FALSE)
laddst_sv_df <- laddst_sv_resp$chargerstations$csmd        #samtliga publika laddstationer i Sverige

#===Väljer ut laddstationer i Dalarnas län och väljer/byter namn på variabler
##ändra county_id till din region

laddst_dalarna <- laddst_sv_df %>%  
  filter(County_ID == 20) %>% 
  select(
    id, 
    namn = name,
    gata = Street,
    gatnr = House_number,
    postnr = Zipcode,
    ort = City,
    kom_kod = Municipality_ID,
    kommun = Municipality,
    lan_kod = County_ID,
    lan = County,
    lages_beskrivning = Description_of_location,
    agare = Owned_by,
    operator = Operator,
    anslutningspunkter = Available_charging_points,
    kommentar = User_comment,
    kontakt = Contact_info,
    skapad = Created,
    uppdaterad = Updated,
    station_status = Station_status,
    Position
  )

#### NVDB från lokal drive G: (hämta från geodatabas i framtid)

nvdb <- "G:/Samhällsanalys/GIS/Grundkartor/nvdb/"
fil_nvdb <- "fixat_nvdb_riket.gpkg"  # oklart vad som är fixat, men filen väger mindre än nvdb_riket

sokvag_nvdb_sv <- paste0(nvdb, fil_nvdb)

nvdb_sv <- st_read(sokvag_nvdb_sv)  

nvdb_dalarna <- nvdb_sv %>% 
  filter(Vagnummer_Lanstillhorighet == 20) %>%         #ändra länstillhörighet till ditt regionid
  select("id" = "id",                                  #och rename
         "antal korfalt" = "Antal_korfalt2_Korfaltsantal", 
         "barighet" = "Barighet_Barighetsklass",
         "vagklass" = "FunkVagklass_Klass", 
         "gatunamn" = "Gatunamn_Namn",
         "hastighet_f" = "Hastighetsgrans_HogstaTillatnaHastighet_F",
         "hastighet_b" = "Hastighetsgrans_HogstaTillatnaHastighet_B",
         "Rekomenderad vag for farligt gods" = "RekomVagFarligtGods_Rekommendation",
         "ars dygns trafik" = "Trafik_ADT_fordon",
         "vag bredd" = "Vagbredd_Bredd", 
         "vaghallare" = "Vaghallare_Vaghallartyp", 
         "vagnr_europavag" = "Vagnummer_Europavag",
         "vagnummer" = "Vagnummer_Huvudnummer_Vard") 

nvdb_dalarna <- nvdb_dalarna %>% 
  mutate(vagnr_europavag = replace(vagnr_europavag, vagnr_europavag == -1, "E"))

# är det möjligt att gruppera på vägnr för att använda burst = vägnr?
##borde vara möjligt med QGIS-algoritm

#mapview(nvdb_dalarna, zcol = "vagnummer", label = "vagnummer", lwd = 3, alpha = 0.2, legend = FALSE) #titta på vägarna i Viewer

#Här hämtas kommun-polygoner från RUF:s geodatabas
##ersätt med din region/kommun (polygoner)

con <- dbConnect(          # use in other settings
  RPostgres::Postgres(),   # funkar ej när jag tar bort denna raden, tänker att den inte behövs eftersom paketet laddas av pacman.
  # without the previous and next lines, some functions fail with bigint data 
  #   so change int64 to integer
  bigint = "integer",  
  user = key_list(service = "postgres")$username,     #användare sparat i lösenordshanterare Keyring
  password = key_get("postgres", key_list(service = "postgres")$username),   #lösenord sparat i lösenordshanterare
  host = "WFALMITVS526.ltdalarna.se",
  port = 5432,
  dbname = "geodata",
  options="-c search_path=public")

kommuner <- st_read(con, query = "SELECT * FROM karta.kommun_scb") 

kommuner_dalarna <- kommuner %>% 
  filter(lanskod_tx == 20) %>%    ##ändra länskod_tx till din regionkod
  select("kom_kod" = "knkod",
         "kommun" = "knnamn",
         "area" = "landareakm",
         "befolkning" = "knbef96")

#mapview(kommuner_dalarna, zcol = "kommun", burst = TRUE, alpha.regions = 0)    #titta på kommunpolygoner i Viewer

# =================== Stäng anslutningen när du är färdig ========================            
dbDisconnect(con)

# ==============Aggregerar laddstationer och anslutningspunkter per kommun  ========

laddstationer_agg <- laddst_dalarna %>% 
  group_by(kom_kod, kommun) %>%
  summarise(sum_anslut_kom = sum(anslutningspunkter), # lägg till na
            sum_station_kom = n()) %>% 
  ungroup()

# lägg ihop kommunpolygonerna med laddstolpar aggregerat till kommun
laddst_anslut_kom <- left_join(kommuner_dalarna, laddstationer_agg, by = "kom_kod") 

laddstationer_kom <- laddst_anslut_kom %>% 
  select(summa_anslutningspunkter = sum_anslut_kom,
         summa_laddstationer = sum_station_kom,
         kommun = kommun.x,
         kom_kod, area, befolkning)

#mapview(laddstationer_kom, alpha.regions = 0, label = "kommun", legend = FALSE)

#skapar punktlagret med laddstationer
#====================punktlagret med laddstationer===============================

laddstationer_punkt <- laddst_dalarna$Position <- gsub("[()]", "", as.character(laddst_dalarna$Position)) #ta bort parenteser

laddstationer_punkt <- laddst_dalarna %>%  separate_wider_delim(Position, ",", names = c("lat", "lon")) #WGS84 Decimal (lat, lon) 

laddstationer_punkt <- st_as_sf(laddstationer_punkt, coords = c("lon", "lat"), 
                                crs = 4326, agr = "constant")
#gör om till SWEREF99TM?
# mapview(laddstationer_punkt)

pal <-  mapviewPalette("mapviewSpectralColors")

greys <- colorRampPalette(c("grey50", "grey87"))

greens <- colorRampPalette(c("darkgreen", "green"))

#oranges <- colorRampPalette(c("orange4", "orange3", "orange2", "orange1", "orange"))

#Skapar kartan

mapview(nvdb_dalarna, zcol = "vagnummer", color = greys, label = "vagnummer", lwd = 3, alpha = 0.5, legend = FALSE, homebutton = FALSE, hide = TRUE, layer.name = "Stora vagar")+
  mapview(laddstationer_punkt, zcol = "anslutningspunkter", legend = FALSE, col.regions = greens(15), cex = "anslutningspunkter", homebutton = FALSE, layer.name = "Publika laddstationer")+
  mapview(laddstationer_kom, zcol = "kommun", alpha.regions = 0, legend = FALSE, hide = TRUE, layer.name = "Kommungranser", homebutton = FALSE)


# layer.name = "Kommungranser"
# homebutton = FALSE,
# hide = TRUE,









