import com.github.nishidy.ParseWikipediaXML.*;

public class Main {

    public static void main(String... args){

        AbstParser parser;
        parser = EngParser.getInstance(args);
        //parser = JapParser.getInstance(args);

        parser.ParseTextToBofw();
        parser.ParseBofwForTfIdf();

    }

}
