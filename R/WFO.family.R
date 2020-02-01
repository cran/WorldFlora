WFO.family <- function(
    taxon=NULL, WFO.file=NULL, WFO.data=NULL, ...
)
{

    .WorldFlora <- new.env()

    bryophytes <- c("Acrobolbaceae", "Adelanthaceae", "Allisoniaceae", "Amblystegiaceae", "Anastrophyllaceae", "Andreaeaceae", "Andreaeobryaceae", "Aneuraceae", "Antheliaceae", "Anthocerotaceae", "Archidiaceae", "Arnelliaceae", "Aulacomniaceae", "Aytoniaceae", "Balantiopsaceae", "Bartramiaceae", "Blasiaceae", "Brachytheciaceae", "Brevianthaceae", "Bruchiaceae", "Bryaceae", "Bryobartramiaceae", "Bryoxiphiaceae", "Buxbaumiaceae", "Calomniaceae", "Calymperaceae", "Calypogeiaceae", "Catagoniaceae", "Catoscopiaceae", "Cephaloziaceae", "Cephaloziellaceae", "Chaetophyllopsaceae", "Chonecoleaceae", "Cinclidotaceae", "Cleveaceae", "Climaciaceae", "Conocephalaceae", "Corsiniaceae", "Cryphaeaceae", "Cyrtopodaceae", "Daltoniaceae", "Dendrocerotaceae", "Dicnemonaceae", "Dicranaceae", "Diphysciaceae", "Disceliaceae", "Ditrichaceae", "Echinodiaceae", "Encalyptaceae", "Entodontaceae", "Ephemeraceae", "Erpodiaceae", "Eustichiaceae", "Exormothecaceae", "Fabroniaceae", "Fissidentaceae", "Fontinalaceae", "Fossombroniaceae", "Funariaceae", "Geocalycaceae", "Gigaspermaceae", "Goebeliellaceae", "Grimmiaceae", "Gymnomitriaceae", "Gyrothyraceae", "Haplomitriaceae", "Hedwigiaceae", "Helicophyllaceae", "Herbertaceae", "Hookeriaceae", "Hylocomiaceae", "Hymenophytaceae", "Hypnaceae", "Hypnodendraceae", "Hypopterygiaceae", "Jackiellaceae", "Jubulaceae", "Jubulopsaceae", "Jungermanniaceae", "Lejeuneaceae", "Lembophyllaceae", "Lepicoleaceae", "Lepidolaenaceae", "Lepidoziaceae", "Leptodontaceae", "Lepyrodontaceae", "Leskeaceae", "Leucodontaceae", "Leucomiaceae", "Lophocoleaceae", "Lophoziaceae", "Lunulariaceae", "Makinoaceae", "Marchantiaceae", "Mastigophoraceae", "Meesiaceae", "Mesoptychiaceae", "Meteoriaceae", "Metzgeriaceae", "Microtheciellaceae", "Mitteniaceae", "Mizutaniaceae", "Mniaceae", "Monocarpaceae", "Monocleaceae", "Monosoleniaceae", "Myriniaceae", "Myuriaceae", "Neckeraceae", "Neotrichocoleaceae", "Notothyladaceae", "Octoblepharaceae", "Oedipodiaceae", "Orthorrhynchiaceae", "Orthotrichaceae", "Oxymitraceae", "Pallaviciniaceae", "Pelliaceae", "Phyllodrepaniaceae", "Phyllogoniaceae", "Pilotrichaceae", "Plagiochilaceae", "Plagiotheciaceae", "Pleurophascaceae", "Pleuroziaceae", "Pleuroziopsaceae", "Polytrichaceae", "Porellaceae", "Pottiaceae", "Prionodontaceae", "Pseudoditrichaceae", "Pseudolepicoleaceae", "Pterigynandraceae", "Pterobryaceae", "Ptilidiaceae", "Ptychomitriaceae", "Ptychomniaceae", "Racopilaceae", "Radulaceae", "Regmatodontaceae", "Rhabdoweisiaceae", "Rhachitheciaceae", "Rhacocarpaceae", "Rhizogoniaceae", "Ricciaceae", "Riellaceae", "Rigodiaceae", "Rutenbergiaceae", "Scapaniaceae", "Schistochilaceae", "Schistostegaceae", "Scorpidiaceae", "Seligeriaceae", "Sematophyllaceae", "Serpotortellaceae", "Sorapillaceae", "Sphaerocarpaceae", "Sphagnaceae", "Spiridentaceae", "Splachnaceae", "Splachnobryaceae", "Stereophyllaceae", "Takakiaceae", "Targioniaceae", "Tetraphidaceae", "Thamnobryaceae", "Theliaceae", "Thuidiaceae", "Timmiaceae", "Trachypodaceae", "Treubiaceae", "Trichocoleaceae", "Trichotemnomataceae", "Vandiemeniaceae", "Vetaformaceae", "Viridivelleraceae", "Wardiaceae", "Wiesnerellaceae")

    if (is.null(WFO.data) == TRUE) {
        message(paste("Reading WFO data"))
        WFO.data <- data.table::fread(WFO.file, encoding="UTF-8")
    }

    WFO.found <- WFO.one(WFO.match(taxon, WFO.file=NULL, WFO.data=WFO.data, ...))
    if (nrow(WFO.found) == 0) {stop("no matches found")}

    taxon.found <- WFO.found$scientificName
    rank.found <- WFO.found$taxonRank
    family.found <- WFO.found$family

    message(paste("Submitted name ", taxon, " was matched with: ", taxon.found, " of family: ", family.found, sep=""))

    if (family.found %in% bryophytes) {return("bryophytes")}

    utils::data("vascular.families", package="WorldFlora", envir=.WorldFlora)
    WFO.families <- eval(as.name("vascular.families"), envir=.WorldFlora)

    family.match <- WFO.families[WFO.families$Family == family.found, ]
    
    if (is.null(family.match) == TRUE) {warning("Family was not matched")}
    
    return(family.match)
    
}

