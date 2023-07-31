import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Lê um ficheiro CSV e cria vários agregadores, um por cada métrica, recebendo três argumentos
 * @author Alexandre Rodrigues 54472
 *
 */
public class Main {


	public static void main(String[] args) {


		if (args.length == 3) {


			try {
				Stream<String> metrics = Files.lines(Paths.get(args[0]));
				Stream<String> dataset = Files.lines(Paths.get(args[1]));

				// Criar um processo por cada métrica.
				List<ProcessWrapper> process = new ArrayList<>();

				metrics.forEach(metric -> {

					ProcessWrapper p = new ProcessWrapper(args[2]);
					p.writeLine(metric);
					process.add(p);
				});

				// Para cada linha de valores...
				dataset.forEach(numbers -> { 

					// ... substituir ',' por ' '
					numbers = numbers.replace(',', ' ');

					String result = "", delimiter = "";

					// ... escrever os valores em cada processo...
					for (ProcessWrapper p : process) {

						p.writeLine(numbers);
						result += delimiter + p.readLine();
						delimiter = ",";
					}

					// e apresentar resultados.
					System.out.println(result);
				});


				for (ProcessWrapper p : process) {
					// mata o processo
					p.kill();
				}

				metrics.close();
				dataset.close();

			} catch (IOException e) {
				System.out.println("Não foi possível ler os ficheiros de input.");
			}
		} else {

			System.out.println("Não foi possível ler os ficheiros de input.");
		}
	}
}
