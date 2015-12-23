import com.github.nishidy.ParseWikipediaXML.*;

public class Main {

    public static void main(String... args){

        AbstParser parser = EngParser.getInstance(args);
        //AbstParser parser = JapParser.getInstance(args);

        parser.ParseTextToBofw();
        parser.ParseBofwForTfIdf();

    }

}
