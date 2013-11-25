module Plugin.Wikla(wiklaPlugin) where

import System.Random
import PluginData

wiklaQuotes :: [String]
wiklaQuotes = [
        "Pääohjelman pahantahtoinen algoritmi.",
        "...tai kutsua metodia joka käynnistää kolmannen maailmansodan.",
        "Koululaiskuri tarvitsee koululaiskuria.",
        "Otus ilostui myöskin pääohjelmasta katsottuna.",
        "...viime kerralla otettiin olutta.",
        "Kymmenen vuotta rakennettu raketti tuhoutui nousun yhteydessä ja syynä oli C-ohjelma.",
        "Teemmepä mitä tahansa, ohjelma tulostaa töttöröö.",
        "Olious ei ole mitään sen syvällisempää, kuin että roikutaan langan päässä.",
        "Olen tässä itseni kanssa harrastanut mielihyvän hankintaa leikkaamalla ja liimaamalla.",
        "Merkittävä vähemmistö, mutta ei lähellekään enemmistö.",
        "...siitä riippuen päivitän nenän jommalle kummalle puolelle. Älkää yrittäkö pelkällä nenällä laskea sitä, vaikka sen voisi laskea pelkällä nenälläkin.",
        "Tätä en haluaisi ohjelmoida autiosaaren rannalla krapulassa.",
        "Jos te kirjotatte whilen tilalle esimerkiksi hilipatihippaa...",
        "Java on kuin ohjelmointikielten saksa.",
        "C on kuin aikamme assembler. Älkää kuitenkaan menkö sanomaan että: \"Wikla sanoi luennolla että C on assembler!\" ...vaikka se onkin.",
        "Nämä suuret luvut tuottavat mulle vaikeuksia...aivan kuten pienetkin.",
        "Parin C:llä alkavan kielen turmiollisesta vaikutuksesta johtuen...",
        "Haluaako joku kuulla miten luontevasti nämä on tehty Scala-kielessä?  ..ei? No, mä kerron silti.",
        "Staticin sekoittaminen periytymiskuvioihin ei ole onnellinen asia.",
        "Kaikilla eläimillä ei ole naukumisaksessoria.",
        "Käyttäjä luulee syöttävänsä kissaa, jonka syöttämismetodi on korvattu tyhjällä metodilla, ja kissa kuolee.",
        "Täällä on buttoneita, JButtoneita, Jenson Buttoneita.",
        "Kyllä luutunsoittajan pitäis saada kolmoisklikkaus aikaan.",
        "Scala ei kuulu kurssiin."
    ]

wiklaQuote :: IO String
wiklaQuote = (randomRIO (0,(length wiklaQuotes)) :: IO Int) >>= \i -> return $ wiklaQuotes !! i

wiklaPlugin :: PluginData a -> IO (PluginResult a)
wiklaPlugin pd = wiklaQuote >>= \q -> msgToChannel pd q
