import org.msgpack.MessagePack;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import org.msgpack.template.Template;
import static org.msgpack.template.Templates.tMap;
import static org.msgpack.template.Templates.TString;
import java.io.*;
import java.util.*;

class WriteDictionaryToMsgpack {

	Map<String,String> mapDict = new HashMap<>();

	void createMapDictionary(String file) throws IOException {

		String line;
		try( BufferedReader br = new BufferedReader(new FileReader(file)) ){
			while((line=br.readLine())!=null){
				if(line.indexOf(";;;")>=0) continue;
				String cols[] = line.split("[ \\t]");
				mapDict.put(cols[0],cols[3]);
			}
		} catch (IOException e){
			throw e;
		}

	}

	public static void main(String argv[]) throws IOException {

		WriteDictionaryToMsgpack m = new WriteDictionaryToMsgpack();
		m.createMapDictionary("../../share/morph_english.flat");


		MessagePack msgpack = new MessagePack();

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		Packer packer = msgpack.createPacker(out);
		packer.write(m.mapDict);

		OutputStream outputStream = new FileOutputStream("../../share/dictionary.msgpack");
		out.writeTo(outputStream);

		// Read it to check
		File file = new File("../../share/dictionary.msgpack");
		byte [] bytes = new byte[(int) file.length() ];;
		InputStream inputStream = new FileInputStream("../../share/dictionary.msgpack");
		inputStream.read(bytes);

		ByteArrayInputStream in = new ByteArrayInputStream(bytes);
		Unpacker unpacker = msgpack.createUnpacker(in);

		Template<Map<String,String>> mapTmpl = tMap(TString,TString);
		Map<String,String> inMapDict = unpacker.read(mapTmpl);

		// Show map to check
		for(Map.Entry<String,String> e : inMapDict.entrySet()){
			System.out.println(e.getKey() + " : " + e.getValue());
		}

	}
}
