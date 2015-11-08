import static spark.Spark.*;
import spark.ModelAndView;
import spark.template.mustache.MustacheTemplateEngine;

import java.util.Map;
import java.util.HashMap;

public class WebView {
	public static void main(String[] args){
		get("/", (request, response) -> {
			Map<String,String> model = new HashMap<>();
			model.put("key","Hello world!");
			return new ModelAndView(model,"views/index.mustache");
		}, new MustacheTemplateEngine());
	}
}
