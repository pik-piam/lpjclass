#' whatis
#' 
#' Helps to understand cropnames and abbreviation used in LPJmL and MAgPIE
#' 
#' 
#' @usage whatis(x)
#' @param x a character with quotes.
#' @return a list with four list elements on: LPJmLName - the LPJmL name,
#' CropAreaLPJmL - the crops whose cultivated areas are contained in the LPJmL
#' landuse input file, CropGroupMAgPIE - the FAO crops whose areas and yields
#' are compared to MAgPIE areas and yield, RepresentativeCropLPJmL - the
#' parameters from this crop are used for the whole group in LPJmL.
#' @author Katharina Waha
#' @seealso \code{\link{readLPJ}},\code{\link{read.LPJ_input}}
#' @export
#' @examples
#' 
#'  whatis("tece")
#' 
whatis <- function(x) {
   help.cropname<-list(
	tece=list(LPJmLName="temperate cereals",CropAreaLPJmL=c("wheat","barley","rye"),
		CropGroupMAgPIE=c("wheat","barley","rye","mixed grain","oats","triticale"),RepresentativeCropLPJmL="wheat"),
	rice=list(LPJmLName="rice",CropAreaLPJmL="paddy rice",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="rice"),
      rice_pro=list(LPJmLName="rice",CropAreaLPJmL="paddy rice",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="rice"),
      maize=list(LPJmLName="maize",CropAreaLPJmL="maize",
		CropGroupMAgPIE=c("maize","popcorn"),RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      maiz=list(LPJmLName="maize",CropAreaLPJmL="maize",
		CropGroupMAgPIE=c("maize","popcorn"),RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      trce=list(LPJmLName="tropical cereals",CropAreaLPJmL=c("millet","sorghum"),
		CropGroupMAgPIE=c("millet","sorghum","canary seed","cereals,nes","fonio","quinoa"),RepresentativeCropLPJmL="millet"),
      pulses=list(LPJmLName="pulses",CropAreaLPJmL=c("bambara beans","beans dry","broad beans dry",
		"chick peas","cow pea dry","lentils","lupins","peas dry","pigeon peas","other pulses",
		"vetches"),CropGroupMAgPIE=c("same as CropAreaLPJmL"),RepresentativeCropLPJmL="field pea"),
      puls_pro=list(LPJmLName="pulses",CropAreaLPJmL=c("bambara beans","beans dry","broad beans dry",
		"chick peas","cow pea dry","lentils","lupins","peas dry","pigeon peas","other pulses",
		"vetches"),CropGroupMAgPIE=c("same as CropAreaLPJmL"),RepresentativeCropLPJmL="field pea"),
	tero=list(LPJmLName="temperate roots",CropAreaLPJmL="sugarbeet",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      sugr_beet=list(LPJmLName="temperate roots",CropAreaLPJmL="sugarbeet",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	trro=list(LPJmLName="tropical roots",CropAreaLPJmL="cassava",
		CropGroupMAgPIE=c("cassava","roots and tubers,nes","sweet potato","taro","yams","yautia(cocoyam)"),
		RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      others=list(LPJmLName="others",CropAreaLPJmL=c("potatoes","oil palm", "citrus", 
		"date palm","grapes,vine","cotton","cocoa","coffee","other perennial crops","other annual crops"),
		CropGroupMAgPIE=c("Apples","Apricots","Avocados","Bananas","Berries Nes","Blueberries","Carobs",
		"Cashewapple","Cherries","Citrus fruit nes","Cranberries","Currants","Dates",
		"Figs","Fruit Fresh Nes","Fruit tropical fresh nes","Gooseberries","Grapefruit (inc. pomelos)",
		"Grapes","Kiwi fruit","Lemons and limes","Mangoes mangosteens guavas","Oranges",
		"Papayas","Peaches and nectarines","Pears","Persimmons","Pineapples","Plantains",
		"Plums and sloes","Quinces","Raspberries","Sour cherries","Stone fruit nes","Strawberries",
		"Tangerines mandarins clem.","Castor oil seed","Coconuts","Kapok Fruit","Kapokseed in Shell",
		"Karite Nuts,Sheanuts","Melonseed","Oilseeds Nes","Olives","Poppy seed","Safflower seed",
		"Sesame seed","Tallowtree Seeds","Tung Nuts","Almonds with shell","Brazil nuts with shell",
		"Cashew nuts with shell","Chestnuts","Hazelnuts with shell","Nuts nes","Pistachios",
		"Walnuts with shell","Artichokes","Asparagus","Beans green","Cabbages and other brassicas",
		"Carrots and turnips","Cauliflowers and broccoli","Chillies and peppers green","Cucumbers and gherkins",
		"Eggplants (aubergines)","Garlic","Leguminous vegetables nes","Lettuce and chicory","Maize green",
		"Mushrooms and truffles","Okra","Onions (inc. shallots) green","Onions dry","Other melons (inc.cantaloupes)",
		"Peas green","Pumpkins squash and gourds","Spinach","String beans","Tomatoes","Vegetables fresh nes",
		"Watermelons","Anise badian fennel corian.","Arecanuts","Chicory roots","Chillies and peppers dry",
		"Cinnamon (canella)","Cloves","Cocoa beans","Coffee green","Ginger","Gums Natural","Hops","Kolanuts",
		"Mat","Natural rubber","Nutmeg mace and cardamoms","Pepper (Piper spp.)","Peppermint","PyrethrumDried",
		"Spices nes","Tea","Tobacco unmanufactured","Vanilla","Cassava leaves","Pome fruit nes","Tea Nes","Arabic Gums"),
		RepresentativeCropLPJmL="managed grassland"),
      cassava_sp=list(LPJmLName="tropical roots",CropAreaLPJmL="cassava",
		CropGroupMAgPIE=c("cassava","roots and tubers,nes","sweet potato","taro","yams","yautia(cocoyam)"),
		RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	sunflower=list(LPJmLName="sunflower",CropAreaLPJmL="sunflower",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	soybean=list(LPJmLName="soybean",CropAreaLPJmL="soybean",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	groundnut=list(LPJmLName="groundnut",CropAreaLPJmL="groundnut",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	rapeseed=list(LPJmLName="rapeseed",CropAreaLPJmL="rapeseed",
		CropGroupMAgPIE=c("rapeseed","mustard seed"),RepresentativeCropLPJmL="same as CropAreaLPJmL"),
	sugarcane=list(LPJmLName="sugarcane",CropAreaLPJmL="sugarcane",
		CropGroupMAgPIE=c("sugarcane","sugar crops,nes"),RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      sugr_cane=list(LPJmLName="sugarcane",CropAreaLPJmL="sugarcane",
		CropGroupMAgPIE=c("sugarcane","sugar crops,nes"),RepresentativeCropLPJmL="same as CropAreaLPJmL"),
      
      oilpalm=list(LPJmLname="NA",CropAreaLPJmL="NA",CropGroupMAgPIE="oil palm fruit",RepresentativeCropLPJmL="NA"),
      potato=list(LPJmLname="NA",CropAreaLPJmL="NA",CropGroupMAgPIE="potato",RepresentativeCropLPJmL="NA"),
      cottn_pro=list(LPJmLname="NA",CropAreaLPJmL="NA",CropGroupMAgPIE="cotton",RepresentativeCropLPJmL="NA"),
      foddr=list(LPJmLname="NA",CropAreaLPJmL="NA",CropGroupMAgPIE="NA",RepresentativeCropLPJmL="NA"),
	mgrass=list(LPJmLName="managed grassland",CropAreaLPJmL="pasture",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="managed grassland"),
      pasture=list(LPJmLName="managed grassland",CropAreaLPJmL="pasture",
		CropGroupMAgPIE="same as CropAreaLPJmL",RepresentativeCropLPJmL="managed grassland"),
	begr=list(LPJmLName="bioenergy grass",CropAreaLPJmL="NA",
		CropGroupMAgPIE="same as RepresentativeCropLPJmL",RepresentativeCropLPJmL=c("miscanthus","switchgrass")),
	betr=list(LPJmLName=c("bioenergy temperate tree","bioenergy tropical tree"),CropAreaLPJmL="NA",
		CropGroupMAgPIE="same as RepresenativeCropLPJmL",RepresentativeCropLPJmL=c("tropical:eucalyptus","temperate:poplar and willow"))
  )
 if (x %in% names(help.cropname)){
   y <- help.cropname[[grep(x,names(help.cropname))]]
   return (y)
 }
 else 
   print("Name of crop not in list yet")
}