package oxbow.exceptions.intro;


import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Optional;

public class JavaCompilerHelpsCaptureRefactoringMistakes {

    private final String pathToFile = "/path/to/file";

    public void example1() {
        try {
            Reader r = findReader();
            slurpReader(r).ifPresent(System.out::println);
        } catch (FileNotFoundException e) {
            System.out.println("No reader found");
        }
    }

    private Optional<String> slurpReader(Reader r) {
        try (BufferedReader b = new BufferedReader(r)) {
            String line;
            StringBuilder buff = new StringBuilder();
            while ((line = b.readLine()) != null) {
                buff.append(line);
            }
            return Optional.of(buff.toString());
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    private Reader findReader() throws FileNotFoundException {
        return new FileReader(pathToFile);
    }


    /**
     * We decide to 'refactor' (OK, change) to using <tt>java.nio.file</tt> API and construct
     * a reader using <tt>Files.newBufferedReader</tt>. The Java compiler helps us by indicating that
     * this throws a different exception type
     */
    public void example2() {
        try {
            Reader r = findReaderNio2(); //2. Then compile error (unless catch block changed)
            slurpReader(r).ifPresent(System.out::println);
        } catch (IOException /* FileNotFoundException */ e) {
            System.out.println("No reader found");
        }
    }

    private Reader findReaderNio2() throws IOException /* FileNotFoundException */ {
        return Files.newBufferedReader(Paths.get(pathToFile)); //1. compile error (must change throws clause)
    }



}
