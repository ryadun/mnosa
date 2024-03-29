library(tibble)

eidcodes <- tribble(
  ~eid, ~city, ~county,
  "100", "Ada", "Norman",
  "101", "Adams", "Mower",
  "102", "Adrian", "Nobles",
  "103", "Afton", "Washington",
  "104", "Aitkin", "Aitkin",
  "105", "Akeley", "Hubbard",
  "106", "Albany", "Stearns",
  "107", "Albert Lea", "Freeborn",
  "108", "Alberta", "Stevens",
  "109", "Albertville", "Wright",
  "110", "Alden", "Freeborn",
  "111", "Aldrich", "Wadena",
  "112", "Alexandria", "Douglas",
  "113", "Alpha", "Jackson",
  "114", "Altura", "Winona",
  "115", "Alvarado", "Marshall",
  "116", "Amboy", "Blue Earth",
  "117", "Andover", "Anoka",
  "118", "Annandale", "Wright",
  "119", "Anoka", "Anoka",
  "120", "Apple Valley", "Dakota",
  "121", "Appleton", "Swift",
  "122", "Arco", "Lincoln",
  "123", "Arden Hills", "Ramsey",
  "124", "Argyle", "Marshall",
  "125", "Arlington", "Sibley",
  "126", "Ashby", "Grant",
  "127", "Askov", "Pine",
  "128", "Atwater", "Kandiyohi",
  "129", "Audubon", "Becker",
  "130", "Aurora", "Saint Louis",
  "131", "Austin", "Mower",
  "132", "Avoca", "Murray",
  "133", "Avon", "Stearns",
  "134", "Babbitt", "Saint Louis",
  "135", "Backus", "Cass",
  "136", "Badger", "Roseau",
  "137", "Bagley", "Clearwater",
  "138", "Balaton", "Lyon",
  "139", "Barnesville", "Clay",
  "140", "Barnum", "Carlton",
  "141", "Barrett", "Grant",
  "142", "Barry", "Big Stone",
  "143", "Battle Lake", "Otter Tail",
  "144", "Baudette", "Lake of The Woods",
  "145", "Baxter", "Crow Wing",
  "146", "Bayport", "Washington",
  "147", "Beardsley", "Big Stone",
  "148", "Beaver Bay", "Lake",
  "149", "Beaver Creek", "Rock",
  "150", "Becker", "Sherburne",
  "151", "Bejou", "Mahnomen",
  "152", "Belgrade", "Stearns",
  "153", "Belle Plaine", "Scott",
  "154", "Bellechester", "Goodhue",
  "155", "Bellingham", "Lac qui Parle",
  "156", "Beltrami", "Polk",
  "157", "Belview", "Redwood",
  "158", "Bemidji", "Beltrami",
  "159", "Bena", "Cass",
  "160", "Benson", "Swift",
  "161", "Bertha", "Todd",
  "162", "Bethel", "Anoka",
  "163", "Big Falls", "Koochiching",
  "164", "Big Lake", "Sherburne",
  "165", "Bigelow", "Nobles",
  "166", "Bigfork", "Itasca",
  "167", "Bingham Lake", "Cottonwood",
  "168", "Birchwood", "Washington",
  "169", "Bird Island", "Renville",
  "170", "Biscay", "McLeod",
  "171", "Biwabik", "Saint Louis",
  "172", "Blackduck", "Beltrami",
  "173", "Blaine", "Anoka",
  "174", "Blomkest", "Kandiyohi",
  "175", "Blooming Prairie", "Steele",
  "176", "Bloomington", "Hennepin",
  "177", "Blue Earth", "Faribault",
  "178", "Bluffton", "Otter Tail",
  "179", "Bock", "Mille Lacs",
  "180", "Borup", "Norman",
  "181", "Bovey", "Itasca",
  "182", "Bowlus", "Morrison",
  "183", "Boy River", "Cass",
  "184", "Boyd", "Lac qui Parle",
  "185", "Braham", "Isanti",
  "186", "Brainerd", "Crow Wing",
  "187", "Brandon", "Douglas",
  "188", "Breckenridge", "Wilkin",
  "189", "Breezy Point", "Crow Wing",
  "190", "Brewster", "Nobles",
  "191", "Bricelyn", "Faribault",
  "192", "Brook Park", "Pine",
  "193", "Brooklyn Center", "Hennepin",
  "194", "Brooklyn Park", "Hennepin",
  "195", "Brooks", "Red Lake",
  "196", "Brookston", "Saint Louis",
  "197", "Brooten", "Stearns",
  "198", "Browerville", "Todd",
  "199", "Browns Valley", "Traverse",
  "200", "Brownsdale", "Mower",
  "201", "Brownsville", "Houston",
  "202", "Brownton", "McLeod",
  "203", "Bruno", "Pine",
  "204", "Buckman", "Morrison",
  "205", "Buffalo", "Wright",
  "206", "Buffalo Lake", "Renville",
  "207", "Buhl", "Saint Louis",
  "208", "Burnsville", "Dakota",
  "209", "Burtrum", "Todd",
  "210", "Butterfield", "Watonwan",
  "211", "Byron", "Olmsted",
  "212", "Caledonia", "Houston",
  "213", "Callaway", "Becker",
  "214", "Calumet", "Itasca",
  "215", "Cambridge", "Isanti",
  "216", "Campbell", "Wilkin",
  "217", "Canby", "Yellow Medicine",
  "218", "Cannon Falls", "Goodhue",
  "219", "Canton", "Fillmore",
  "220", "Carlos", "Douglas",
  "221", "Carlton", "Carlton",
  "222", "Carver", "Carver",
  "223", "Cass Lake", "Cass",
  "224", "Cedar Mills", "Meeker",
  "225", "Center City", "Chisago",
  "226", "Centerville", "Anoka",
  "227", "Ceylon", "Martin",
  "228", "Champlin", "Hennepin",
  "229", "Chandler", "Murray",
  "230", "Chanhassen", "Carver",
  "231", "Chaska", "Carver",
  "232", "Chatfield", "Fillmore",
  "233", "Chickamaw Beach", "Cass",
  "234", "Chisago City", "Chisago",
  "235", "Chisholm", "Saint Louis",
  "236", "Chokio", "Stevens",
  "237", "Circle Pines", "Anoka",
  "238", "Clara City", "Chippewa",
  "239", "Claremont", "Dodge",
  "240", "Clarissa", "Todd",
  "241", "Clarkfield", "Yellow Medicine",
  "242", "Clarks Grove", "Freeborn",
  "243", "Clear Lake", "Sherburne",
  "244", "Clearbrook", "Clearwater",
  "245", "Clearwater", "Wright",
  "246", "Clements", "Redwood",
  "247", "Cleveland", "Le Sueur",
  "248", "Climax", "Polk",
  "249", "Clinton", "Big Stone",
  "250", "Clitherall", "Otter Tail",
  "251", "Clontarf", "Swift",
  "252", "Cloquet", "Carlton",
  "253", "Coates", "Dakota",
  "254", "Cobden", "Brown",
  "255", "Cohasset", "Itasca",
  "256", "Cokato", "Wright",
  "257", "Cold Spring", "Stearns",
  "258", "Coleraine", "Itasca",
  "259", "Cologne", "Carver",
  "260", "Columbia Heights", "Anoka",
  "954", "Columbus", "Anoka",
  "261", "Comfrey", "Brown",
  "262", "Comstock", "Clay",
  "263", "Conger", "Freeborn",
  "264", "Cook", "Saint Louis",
  "265", "Coon Rapids", "Anoka",
  "266", "Corcoran", "Hennepin",
  "267", "Correll", "Big Stone",
  "268", "Cosmos", "Meeker",
  "269", "Cottage Grove", "Washington",
  "270", "Cottonwood", "Lyon",
  "271", "Courtland", "Nicollet",
  "272", "Cromwell", "Carlton",
  "273", "Crookston", "Polk",
  "274", "Crosby", "Crow Wing",
  "275", "Crosslake", "Crow Wing",
  "276", "Crystal", "Hennepin",
  "277", "Currie", "Murray",
  "278", "Cuyuna", "Crow Wing",
  "279", "Cyrus", "Pope",
  "280", "Dakota", "Winona",
  "281", "Dalton", "Otter Tail",
  "282", "Danube", "Renville",
  "283", "Danvers", "Swift",
  "284", "Darfur", "Watonwan",
  "285", "Darwin", "Meeker",
  "286", "Dassel", "Meeker",
  "287", "Dawson", "Lac qui Parle",
  "288", "Dayton", "Hennepin",
  "289", "De Graff", "Swift",
  "290", "Deephaven", "Hennepin",
  "291", "Deer Creek", "Otter Tail",
  "292", "Deer River", "Itasca",
  "293", "Deerwood", "Crow Wing",
  "294", "Delano", "Wright",
  "295", "Delavan", "Faribault",
  "296", "Delhi", "Redwood",
  "297", "Dellwood", "Washington",
  "298", "Denham", "Pine",
  "299", "Dennison", "Goodhue",
  "300", "Dent", "Otter Tail",
  "301", "Detroit Lakes", "Becker",
  "302", "Dexter", "Mower",
  "303", "Dilworth", "Clay",
  "304", "Dodge Center", "Dodge",
  "305", "Donaldson", "Kittson",
  "306", "Donnelly", "Stevens",
  "307", "Doran", "Wilkin",
  "308", "Dover", "Olmsted",
  "309", "Dovray", "Murray",
  "310", "Duluth", "Saint Louis",
  "311", "Dumont", "Traverse",
  "312", "Dundas", "Rice",
  "313", "Dundee", "Nobles",
  "314", "Dunnell", "Martin",
  "315", "Eagan", "Dakota",
  "316", "Eagle Bend", "Todd",
  "317", "Eagle Lake", "Blue Earth",
  "318", "East Bethel", "Anoka",
  "319", "East Grand Forks", "Polk",
  "320", "East Gull Lake", "Cass",
  "321", "Easton", "Faribault",
  "322", "Echo", "Yellow Medicine",
  "323", "Eden Prairie", "Hennepin",
  "324", "Eden Valley", "Meeker",
  "325", "Edgerton", "Pipestone",
  "326", "Edina", "Hennepin",
  "327", "Effie", "Itasca",
  "328", "Eitzen", "Houston",
  "329", "Elba", "Winona",
  "330", "Elbow Lake", "Grant",
  "331", "Elgin", "Wabasha",
  "332", "Elizabeth", "Otter Tail",
  "333", "Elk River", "Sherburne",
  "956", "Elko New Market", "Scott",
  "335", "Elkton", "Mower",
  "336", "Ellendale", "Steele",
  "337", "Ellsworth", "Nobles",
  "338", "Elmdale", "Morrison",
  "339", "Elmore", "Faribault",
  "340", "Elrosa", "Stearns",
  "341", "Ely", "Saint Louis",
  "342", "Elysian", "Le Sueur",
  "343", "Emily", "Crow Wing",
  "344", "Emmons", "Freeborn",
  "345", "Erhard", "Otter Tail",
  "346", "Erskine", "Polk",
  "347", "Evan", "Brown",
  "348", "Evansville", "Douglas",
  "349", "Eveleth", "Saint Louis",
  "350", "Excelsior", "Hennepin",
  "351", "Eyota", "Olmsted",
  "352", "Fairfax", "Renville",
  "353", "Fairmont", "Martin",
  "354", "Falcon Heights", "Ramsey",
  "355", "Faribault", "Rice",
  "356", "Farmington", "Dakota",
  "357", "Farwell", "Pope",
  "358", "Federal Dam", "Cass",
  "359", "Felton", "Clay",
  "360", "Fergus Falls", "Otter Tail",
  "361", "Fertile", "Polk",
  "362", "Fifty Lakes", "Crow Wing",
  "363", "Finlayson", "Pine",
  "364", "Fisher", "Polk",
  "365", "Flensburg", "Morrison",
  "366", "Floodwood", "Saint Louis",
  "367", "Florence", "Lyon",
  "368", "Foley", "Benton",
  "369", "Forada", "Douglas",
  "370", "Forest Lake", "Washington",
  "371", "Foreston", "Mille Lacs",
  "372", "Fort Ripley", "Crow Wing",
  "373", "Fosston", "Polk",
  "374", "Fountain", "Fillmore",
  "375", "Foxhome", "Wilkin",
  "376", "Franklin", "Renville",
  "377", "Frazee", "Becker",
  "378", "Freeborn", "Freeborn",
  "379", "Freeport", "Stearns",
  "380", "Fridley", "Anoka",
  "381", "Frost", "Faribault",
  "382", "Fulda", "Murray",
  "383", "Funkley", "Beltrami",
  "384", "Garfield", "Douglas",
  "385", "Garrison", "Crow Wing",
  "386", "Garvin", "Lyon",
  "387", "Gary", "Norman",
  "388", "Gaylord", "Sibley",
  "389", "Gem Lake", "Ramsey",
  "390", "Geneva", "Freeborn",
  "391", "Genola", "Morrison",
  "392", "Georgetown", "Clay",
  "393", "Ghent", "Lyon",
  "394", "Gibbon", "Sibley",
  "395", "Gilbert", "Saint Louis",
  "396", "Gilman", "Benton",
  "397", "Glencoe", "McLeod",
  "398", "Glenville", "Freeborn",
  "399", "Glenwood", "Pope",
  "400", "Glyndon", "Clay",
  "401", "Golden Valley", "Hennepin",
  "402", "Gonvick", "Clearwater",
  "403", "Good Thunder", "Blue Earth",
  "404", "Goodhue", "Goodhue",
  "405", "Goodridge", "Pennington",
  "406", "Goodview", "Winona",
  "407", "Graceville", "Big Stone",
  "408", "Granada", "Martin",
  "409", "Grand Marais", "Cook",
  "410", "Grand Meadow", "Mower",
  "411", "Grand Rapids", "Itasca",
  "412", "Granite Falls", "Yellow Medicine",
  "413", "Grant", "Washington",
  "414", "Grasston", "Kanabec",
  "415", "Green Isle", "Sibley",
  "416", "Greenbush", "Roseau",
  "417", "Greenfield", "Hennepin",
  "418", "Greenwald", "Stearns",
  "419", "Greenwood", "Hennepin",
  "420", "Grey Eagle", "Todd",
  "421", "Grove City", "Meeker",
  "422", "Grygla", "Marshall",
  "423", "Gully", "Polk",
  "424", "Hackensack", "Cass",
  "425", "Hadley", "Murray",
  "426", "Hallock", "Kittson",
  "427", "Halma", "Kittson",
  "428", "Halstad", "Norman",
  "429", "Ham Lake", "Anoka",
  "430", "Hamburg", "Carver",
  "431", "Hammond", "Wabasha",
  "432", "Hampton", "Dakota",
  "433", "Hancock", "Stevens",
  "434", "Hanley Falls", "Yellow Medicine",
  "435", "Hanover", "Wright",
  "436", "Hanska", "Brown",
  "437", "Harding", "Morrison",
  "438", "Hardwick", "Rock",
  "439", "Harmony", "Fillmore",
  "440", "Harris", "Chisago",
  "441", "Hartland", "Freeborn",
  "442", "Hastings", "Dakota",
  "443", "Hatfield", "Pipestone",
  "444", "Hawley", "Clay",
  "445", "Hayfield", "Dodge",
  "446", "Hayward", "Freeborn",
  "447", "Hazel Run", "Yellow Medicine",
  "448", "Hector", "Renville",
  "449", "Heidelberg", "Le Sueur",
  "450", "Henderson", "Sibley",
  "451", "Hendricks", "Lincoln",
  "452", "Hendrum", "Norman",
  "453", "Henning", "Otter Tail",
  "454", "Henriette", "Pine",
  "455", "Herman", "Grant",
  "456", "Hermantown", "Saint Louis",
  "457", "Heron Lake", "Jackson",
  "458", "Hewitt", "Todd",
  "459", "Hibbing", "Saint Louis",
  "460", "Hill City", "Aitkin",
  "461", "Hillman", "Morrison",
  "462", "Hills", "Rock",
  "463", "Hilltop", "Anoka",
  "464", "Hinckley", "Pine",
  "465", "Hitterdal", "Clay",
  "466", "Hoffman", "Grant",
  "467", "Hokah", "Houston",
  "468", "Holdingford", "Stearns",
  "469", "Holland", "Pipestone",
  "470", "Hollandale", "Freeborn",
  "471", "Holloway", "Swift",
  "472", "Holt", "Marshall",
  "473", "Hopkins", "Hennepin",
  "474", "Houston", "Houston",
  "475", "Howard Lake", "Wright",
  "476", "Hoyt Lakes", "Saint Louis",
  "477", "Hugo", "Washington",
  "478", "Humboldt", "Kittson",
  "479", "Hutchinson", "McLeod",
  "480", "Ihlen", "Pipestone",
  "481", "Independence", "Hennepin",
  "482", "International Falls", "Koochiching",
  "483", "Inver Grove Heights", "Dakota",
  "484", "Iona", "Murray",
  "485", "Iron Junction", "Saint Louis",
  "486", "Ironton", "Crow Wing",
  "487", "Isanti", "Isanti",
  "488", "Isle", "Mille Lacs",
  "489", "Ivanhoe", "Lincoln",
  "490", "Jackson", "Jackson",
  "491", "Janesville", "Waseca",
  "492", "Jasper", "Pipestone",
  "493", "Jeffers", "Cottonwood",
  "494", "Jenkins", "Crow Wing",
  "495", "Johnson", "Big Stone",
  "496", "Jordan", "Scott",
  "497", "Kandiyohi", "Kandiyohi",
  "498", "Karlstad", "Kittson",
  "499", "Kasota", "Le Sueur",
  "500", "Kasson", "Dodge",
  "501", "Keewatin", "Itasca",
  "502", "Kelliher", "Beltrami",
  "503", "Kellogg", "Wabasha",
  "504", "Kennedy", "Kittson",
  "505", "Kenneth", "Rock",
  "506", "Kensington", "Douglas",
  "507", "Kent", "Wilkin",
  "508", "Kenyon", "Goodhue",
  "509", "Kerkhoven", "Swift",
  "510", "Kerrick", "Pine",
  "511", "Kettle River", "Carlton",
  "512", "Kiester", "Faribault",
  "513", "Kilkenny", "Le Sueur",
  "514", "Kimball", "Stearns",
  "515", "Kinbrae", "Nobles",
  "516", "Kingston", "Meeker",
  "517", "Kinney", "Saint Louis",
  "518", "La Crescent", "Houston",
  "519", "La Prairie", "Itasca",
  "520", "La Salle", "Watonwan",
  "521", "Lafayette", "Nicollet",
  "522", "Lake Benton", "Lincoln",
  "523", "Lake Bronson", "Kittson",
  "524", "Lake City", "Wabasha",
  "525", "Lake Crystal", "Blue Earth",
  "526", "Lake Elmo", "Washington",
  "527", "Lake Henry", "Stearns",
  "528", "Lake Lillian", "Kandiyohi",
  "529", "Lake Park", "Becker",
  "531", "Lake Saint Croix Beach", "Washington",
  "530", "Lake Shore", "Cass",
  "532", "Lake Wilson", "Murray",
  "533", "Lakefield", "Jackson",
  "534", "Lakeland", "Washington",
  "535", "Lakeland Shores", "Washington",
  "536", "Lakeville", "Dakota",
  "537", "Lamberton", "Redwood",
  "538", "Lancaster", "Kittson",
  "539", "Landfall", "Washington",
  "540", "Lanesboro", "Fillmore",
  "541", "Laporte", "Hubbard",
  "542", "Lastrup", "Morrison",
  "543", "Lauderdale", "Ramsey",
  "544", "Le Center", "Le Sueur",
  "545", "Le Sueur", "Le Sueur",
  "546", "Lengby", "Polk",
  "547", "Leonard", "Clearwater",
  "548", "Leonidas", "Saint Louis",
  "549", "LeRoy", "Mower",
  "550", "Lester Prairie", "McLeod",
  "551", "Lewiston", "Winona",
  "552", "Lewisville", "Watonwan",
  "553", "Lexington", "Anoka",
  "554", "Lilydale", "Dakota",
  "555", "Lindstrom", "Chisago",
  "556", "Lino Lakes", "Anoka",
  "557", "Lismore", "Nobles",
  "558", "Litchfield", "Meeker",
  "559", "Little Canada", "Ramsey",
  "560", "Little Falls", "Morrison",
  "561", "Littlefork", "Koochiching",
  "562", "Long Beach", "Pope",
  "563", "Long Lake", "Hennepin",
  "564", "Long Prairie", "Todd",
  "565", "Longville", "Cass",
  "566", "Lonsdale", "Rice",
  "567", "Loretto", "Hennepin",
  "568", "Louisburg", "Lac qui Parle",
  "569", "Lowry", "Pope",
  "570", "Lucan", "Redwood",
  "571", "Luverne", "Rock",
  "572", "Lyle", "Mower",
  "573", "Lynd", "Lyon",
  "574", "Mabel", "Fillmore",
  "575", "Madelia", "Watonwan",
  "576", "Madison", "Lac qui Parle",
  "577", "Madison Lake", "Blue Earth",
  "578", "Magnolia", "Rock",
  "579", "Mahnomen", "Mahnomen",
  "580", "Mahtomedi", "Washington",
  "581", "Manchester", "Freeborn",
  "582", "Manhattan Beach", "Crow Wing",
  "583", "Mankato", "Blue Earth",
  "584", "Mantorville", "Dodge",
  "585", "Maple Grove", "Hennepin",
  "586", "Maple Lake", "Wright",
  "587", "Maple Plain", "Hennepin",
  "588", "Mapleton", "Blue Earth",
  "589", "Mapleview", "Mower",
  "590", "Maplewood", "Ramsey",
  "591", "Marble", "Itasca",
  "592", "Marietta", "Lac qui Parle",
  "593", "Marine on Saint Croix", "Washington",
  "594", "Marshall", "Lyon",
  "595", "Mayer", "Carver",
  "596", "Maynard", "Chippewa",
  "597", "Mazeppa", "Wabasha",
  "598", "McGrath", "Aitkin",
  "599", "McGregor", "Aitkin",
  "600", "McIntosh", "Polk",
  "601", "McKinley", "Saint Louis",
  "602", "Meadowlands", "Saint Louis",
  "603", "Medford", "Steele",
  "604", "Medicine Lake", "Hennepin",
  "605", "Medina", "Hennepin",
  "606", "Meire Grove", "Stearns",
  "607", "Melrose", "Stearns",
  "608", "Menahga", "Wadena",
  "609", "Mendota", "Dakota",
  "610", "Mendota Heights", "Dakota",
  "611", "Mentor", "Polk",
  "612", "Middle River", "Marshall",
  "613", "Miesville", "Dakota",
  "614", "Milaca", "Mille Lacs",
  "615", "Milan", "Chippewa",
  "616", "Millerville", "Douglas",
  "617", "Millville", "Wabasha",
  "618", "Milroy", "Redwood",
  "619", "Miltona", "Douglas",
  "620", "Minneapolis", "Hennepin",
  "621", "Minneiska", "Wabasha",
  "622", "Minneota", "Lyon",
  "623", "Minnesota City", "Winona",
  "624", "Minnesota Lake", "Faribault",
  "625", "Minnetonka", "Hennepin",
  "626", "Minnetonka Beach", "Hennepin",
  "627", "Minnetrista", "Hennepin",
  "628", "Mizpah", "Koochiching",
  "629", "Montevideo", "Chippewa",
  "630", "Montgomery", "Le Sueur",
  "631", "Monticello", "Wright",
  "632", "Montrose", "Wright",
  "633", "Moorhead", "Clay",
  "634", "Moose Lake", "Carlton",
  "635", "Mora", "Kanabec",
  "636", "Morgan", "Redwood",
  "637", "Morris", "Stevens",
  "638", "Morristown", "Rice",
  "639", "Morton", "Renville",
  "640", "Motley", "Morrison",
  "641", "Mound", "Hennepin",
  "642", "Mounds View", "Ramsey",
  "643", "Mountain Iron", "Saint Louis",
  "644", "Mountain Lake", "Cottonwood",
  "645", "Murdock", "Swift",
  "646", "Myrtle", "Freeborn",
  "647", "Nashua", "Wilkin",
  "648", "Nashwauk", "Itasca",
  "649", "Nassau", "Lac qui Parle",
  "650", "Nelson", "Douglas",
  "651", "Nerstrand", "Rice",
  "652", "Nevis", "Hubbard",
  "653", "New Auburn", "Sibley",
  "654", "New Brighton", "Ramsey",
  "655", "New Germany", "Carver",
  "656", "New Hope", "Hennepin",
  "657", "New London", "Kandiyohi",
  "659", "New Munich", "Stearns",
  "660", "New Prague", "Scott",
  "661", "New Richland", "Waseca",
  "662", "New Trier", "Dakota",
  "663", "New Ulm", "Brown",
  "664", "New York Mills", "Otter Tail",
  "665", "Newfolden", "Marshall",
  "666", "Newport", "Washington",
  "667", "Nicollet", "Nicollet",
  "668", "Nielsville", "Polk",
  "669", "Nimrod", "Wadena",
  "670", "Nisswa", "Crow Wing",
  "671", "Norcross", "Grant",
  "672", "North Branch", "Chisago",
  "673", "North Mankato", "Nicollet",
  "674", "North Oaks", "Ramsey",
  "675", "North Saint Paul", "Ramsey",
  "676", "Northfield", "Rice",
  "677", "Northome", "Koochiching",
  "678", "Northrop", "Martin",
  "679", "Norwood Young America", "Carver",
  "957", "Nowthen", "Anoka",
  "680", "Oak Grove", "Anoka",
  "681", "Oak Park Heights", "Washington",
  "682", "Oakdale", "Washington",
  "683", "Odessa", "Big Stone",
  "684", "Odin", "Watonwan",
  "685", "Ogema", "Becker",
  "686", "Ogilvie", "Kanabec",
  "687", "Okabena", "Jackson",
  "688", "Oklee", "Red Lake",
  "689", "Olivia", "Renville",
  "690", "Onamia", "Mille Lacs",
  "691", "Ormsby", "Watonwan",
  "692", "Orono", "Hennepin",
  "693", "Oronoco", "Olmsted",
  "694", "Orr", "Saint Louis",
  "695", "Ortonville", "Big Stone",
  "696", "Osakis", "Douglas",
  "697", "Oslo", "Marshall",
  "698", "Osseo", "Hennepin",
  "699", "Ostrander", "Fillmore",
  "700", "Otsego", "Wright",
  "701", "Ottertail", "Otter Tail",
  "702", "Owatonna", "Steele",
  "703", "Palisade", "Aitkin",
  "704", "Park Rapids", "Hubbard",
  "705", "Parkers Prairie", "Otter Tail",
  "706", "Paynesville", "Stearns",
  "707", "Pease", "Mille Lacs",
  "708", "Pelican Rapids", "Otter Tail",
  "709", "Pemberton", "Blue Earth",
  "710", "Pennock", "Kandiyohi",
  "711", "Pequot Lakes", "Crow Wing",
  "712", "Perham", "Otter Tail",
  "713", "Perley", "Norman",
  "714", "Peterson", "Fillmore",
  "715", "Pierz", "Morrison",
  "716", "Pillager", "Cass",
  "717", "Pine City", "Pine",
  "718", "Pine Island", "Goodhue",
  "719", "Pine River", "Cass",
  "720", "Pine Springs", "Washington",
  "721", "Pipestone", "Pipestone",
  "722", "Plainview", "Wabasha",
  "723", "Plato", "McLeod",
  "725", "Plummer", "Red Lake",
  "726", "Plymouth", "Hennepin",
  "727", "Porter", "Yellow Medicine",
  "728", "Preston", "Fillmore",
  "729", "Princeton", "Mille Lacs",
  "730", "Prinsburg", "Kandiyohi",
  "731", "Prior Lake", "Scott",
  "732", "Proctor", "Saint Louis",
  "733", "Quamba", "Kanabec",
  "734", "Racine", "Mower",
  "735", "Ramsey", "Anoka",
  "736", "Randall", "Morrison",
  "737", "Randolph", "Dakota",
  "738", "Ranier", "Koochiching",
  "739", "Raymond", "Kandiyohi",
  "740", "Red Lake Falls", "Red Lake",
  "741", "Red Wing", "Goodhue",
  "742", "Redwood Falls", "Redwood",
  "743", "Regal", "Kandiyohi",
  "744", "Remer", "Cass",
  "745", "Renville", "Renville",
  "746", "Revere", "Redwood",
  "747", "Rice", "Benton",
  "958", "Rice Lake", "Saint Louis",
  "748", "Richfield", "Hennepin",
  "749", "Richmond", "Stearns",
  "750", "Richville", "Otter Tail",
  "751", "Riverton", "Crow Wing",
  "752", "Robbinsdale", "Hennepin",
  "753", "Rochester", "Olmsted",
  "754", "Rock Creek", "Pine",
  "755", "Rockford", "Wright",
  "756", "Rockville", "Stearns",
  "757", "Rogers", "Hennepin",
  "758", "Rollingstone", "Winona",
  "760", "Roosevelt", "Roseau",
  "761", "Roscoe", "Stearns",
  "762", "Rose Creek", "Mower",
  "763", "Roseau", "Roseau",
  "764", "Rosemount", "Dakota",
  "765", "Roseville", "Ramsey",
  "766", "Rothsay", "Wilkin",
  "767", "Round Lake", "Nobles",
  "768", "Royalton", "Morrison",
  "769", "Rush City", "Chisago",
  "770", "Rushford", "Fillmore",
  "771", "Rushford Village", "Fillmore",
  "772", "Rushmore", "Nobles",
  "773", "Russell", "Lyon",
  "774", "Ruthton", "Pipestone",
  "775", "Rutledge", "Pine",
  "776", "Sabin", "Clay",
  "777", "Sacred Heart", "Renville",
  "813", "Saint Anthony", "Hennepin",
  "814", "Saint Anthony [Stearns]", "Stearns",
  "953", "Saint Augusta", "Stearns",
  "815", "Saint Bonifacius", "Hennepin",
  "816", "Saint Charles", "Winona",
  "817", "Saint Clair", "Blue Earth",
  "818", "Saint Cloud", "Stearns",
  "819", "Saint Francis", "Anoka",
  "820", "Saint Hilaire", "Pennington",
  "821", "Saint James", "Watonwan",
  "822", "Saint Joseph", "Stearns",
  "823", "Saint Leo", "Yellow Medicine",
  "824", "Saint Louis Park", "Hennepin",
  "825", "Saint Martin", "Stearns",
  "826", "Saint Mary&#39;s Point", "Washington",
  "827", "Saint Michael", "Wright",
  "828", "Saint Paul", "Ramsey",
  "829", "Saint Paul Park", "Washington",
  "830", "Saint Peter", "Nicollet",
  "831", "Saint Rosa", "Stearns",
  "832", "Saint Stephen", "Stearns",
  "833", "Saint Vincent", "Kittson",
  "778", "Sanborn", "Redwood",
  "779", "Sandstone", "Pine",
  "780", "Sargeant", "Mower",
  "781", "Sartell", "Stearns",
  "782", "Sauk Centre", "Stearns",
  "783", "Sauk Rapids", "Benton",
  "784", "Savage", "Scott",
  "955", "Scandia", "Washington",
  "785", "Scanlon", "Carlton",
  "786", "Seaforth", "Redwood",
  "787", "Sebeka", "Wadena",
  "788", "Sedan", "Pope",
  "789", "Shafer", "Chisago",
  "790", "Shakopee", "Scott",
  "791", "Shelly", "Norman",
  "792", "Sherburn", "Martin",
  "793", "Shevlin", "Clearwater",
  "794", "Shoreview", "Ramsey",
  "795", "Shorewood", "Hennepin",
  "796", "Silver Bay", "Lake",
  "797", "Silver Lake", "McLeod",
  "798", "Skyline", "Blue Earth",
  "799", "Slayton", "Murray",
  "800", "Sleepy Eye", "Brown",
  "801", "Sobieski", "Morrison",
  "802", "Solway", "Beltrami",
  "803", "South Haven", "Wright",
  "804", "South Saint Paul", "Dakota",
  "805", "Spicer", "Kandiyohi",
  "806", "Spring Grove", "Houston",
  "807", "Spring Hill", "Stearns",
  "808", "Spring Lake Park", "Anoka",
  "809", "Spring Park", "Hennepin",
  "810", "Spring Valley", "Fillmore",
  "811", "Springfield", "Brown",
  "812", "Squaw Lake", "Itasca",
  "834", "Stacy", "Chisago",
  "835", "Staples", "Todd",
  "836", "Starbuck", "Pope",
  "837", "Steen", "Rock",
  "838", "Stephen", "Marshall",
  "839", "Stewart", "McLeod",
  "840", "Stewartville", "Olmsted",
  "841", "Stillwater", "Washington",
  "842", "Stockton", "Winona",
  "843", "Storden", "Cottonwood",
  "844", "Strandquist", "Marshall",
  "845", "Strathcona", "Roseau",
  "846", "Sturgeon Lake", "Pine",
  "847", "Sunburg", "Kandiyohi",
  "848", "Sunfish Lake", "Dakota",
  "849", "Swanville", "Morrison",
  "850", "Taconite", "Itasca",
  "851", "Tamarack", "Aitkin",
  "852", "Taopi", "Mower",
  "853", "Taunton", "Lyon",
  "854", "Taylors Falls", "Chisago",
  "856", "Tenstrike", "Beltrami",
  "857", "Thief River Falls", "Pennington",
  "859", "Tintah", "Traverse",
  "860", "Tonka Bay", "Hennepin",
  "861", "Tower", "Saint Louis",
  "862", "Tracy", "Lyon",
  "863", "Trail", "Polk",
  "864", "Trimont", "Martin",
  "865", "Trommald", "Crow Wing",
  "866", "Trosky", "Pipestone",
  "867", "Truman", "Martin",
  "868", "Turtle River", "Beltrami",
  "869", "Twin Lakes", "Freeborn",
  "870", "Twin Valley", "Norman",
  "871", "Two Harbors", "Lake",
  "872", "Tyler", "Lincoln",
  "873", "Ulen", "Clay",
  "874", "Underwood", "Otter Tail",
  "875", "Upsala", "Morrison",
  "876", "Urbank", "Otter Tail",
  "877", "Utica", "Winona",
  "878", "Vadnais Heights", "Ramsey",
  "879", "Vergas", "Otter Tail",
  "880", "Vermillion", "Dakota",
  "881", "Verndale", "Wadena",
  "882", "Vernon Center", "Blue Earth",
  "883", "Vesta", "Redwood",
  "884", "Victoria", "Carver",
  "885", "Viking", "Marshall",
  "886", "Villard", "Pope",
  "887", "Vining", "Otter Tail",
  "888", "Virginia", "Saint Louis",
  "889", "Wabasha", "Wabasha",
  "890", "Wabasso", "Redwood",
  "891", "Waconia", "Carver",
  "892", "Wadena", "Wadena",
  "893", "Wahkon", "Mille Lacs",
  "894", "Waite Park", "Stearns",
  "895", "Waldorf", "Waseca",
  "896", "Walker", "Cass",
  "897", "Walnut Grove", "Redwood",
  "898", "Walters", "Faribault",
  "899", "Waltham", "Mower",
  "900", "Wanamingo", "Goodhue",
  "901", "Wanda", "Redwood",
  "902", "Warba", "Itasca",
  "903", "Warren", "Marshall",
  "904", "Warroad", "Roseau",
  "905", "Waseca", "Waseca",
  "906", "Watertown", "Carver",
  "907", "Waterville", "Le Sueur",
  "908", "Watkins", "Meeker",
  "909", "Watson", "Chippewa",
  "910", "Waubun", "Mahnomen",
  "911", "Waverly", "Wright",
  "912", "Wayzata", "Hennepin",
  "913", "Welcome", "Martin",
  "914", "Wells", "Faribault",
  "915", "Wendell", "Grant",
  "916", "West Concord", "Dodge",
  "917", "West Saint Paul", "Dakota",
  "918", "West Union", "Todd",
  "919", "Westbrook", "Cottonwood",
  "920", "Westport", "Pope",
  "921", "Whalan", "Fillmore",
  "922", "Wheaton", "Traverse",
  "923", "White Bear Lake", "Ramsey",
  "924", "Wilder", "Jackson",
  "925", "Willernie", "Washington",
  "926", "Williams", "Lake of The Woods",
  "927", "Willmar", "Kandiyohi",
  "928", "Willow River", "Pine",
  "929", "Wilmont", "Nobles",
  "930", "Wilton", "Beltrami",
  "931", "Windom", "Cottonwood",
  "932", "Winger", "Polk",
  "933", "Winnebago", "Faribault",
  "934", "Winona", "Winona",
  "935", "Winsted", "McLeod",
  "936", "Winthrop", "Sibley",
  "937", "Winton", "Saint Louis",
  "938", "Wolf Lake", "Becker",
  "939", "Wolverton", "Wilkin",
  "940", "Wood Lake", "Yellow Medicine",
  "941", "Woodbury", "Washington",
  "942", "Woodland", "Hennepin",
  "943", "Woodstock", "Pipestone",
  "944", "Worthington", "Nobles",
  "945", "Wrenshall", "Carlton",
  "946", "Wright", "Carlton",
  "947", "Wykoff", "Fillmore",
  "948", "Wyoming", "Chisago",
  "949", "Zemple", "Itasca",
  "950", "Zimmerman", "Sherburne",
  "951", "Zumbro Falls", "Wabasha"
)

usethis::use_data(eidcodes, overwrite = TRUE)
