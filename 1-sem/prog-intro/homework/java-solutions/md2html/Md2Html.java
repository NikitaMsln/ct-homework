package md2html;

import java.io.*;
import java.util.*;

public class Md2Html {
    private final static Map<String, String[]> md2InlineHtml = new HashMap<>(){
        {
            put("*", new String[]{"<em>", "</em>"});
            put("_", new String[]{"<em>", "</em>"});
            put("__", new String[]{"<strong>", "</strong>"});
            put("**", new String[]{"<strong>", "</strong>"});
            put("--", new String[]{"<s>", "</s>"});
            put("`", new String[]{"<code>", "</code>"});
            put("<", new String[]{"&lt;"});
            put(">", new String[]{"&gt;"});
            put("&", new String[]{"&amp;"});
            put("\\_", new String[]{"_"});
            put("\\*", new String[]{"*"});
            put("[", new String[]{"<a href='", "'>", "</a>"});
            put("]", new String[]{"<a href='", "'>", "</a>"});
        }
    };

    private final static Map<String, String> pairMd = new HashMap<>(){
        {
            put("[", "]");
            put("]", "[");
        }
    };

    private final static Map<String, String[]> md2LineHtml = new HashMap<>(){
        {
            put("# ", new String[]{"<h1>", "</h1>"});
            put("## ", new String[]{"<h2>", "</h2>"});
            put("### ", new String[]{"<h3>", "</h3>"});
            put("#### ", new String[]{"<h4>", "</h4>"});
            put("##### ", new String[]{"<h5>", "</h5>"});
            put("###### ", new String[]{"<h6>", "</h6>"});
            put(null, new String[]{"<p>", "</p>"});
        }
    };

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Please enter input and output filenames");
            return;
        }

        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), "UTF8"));
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "UTF8"));

            String line;

            String closingTag = null;
            Map<String, Integer> beginOfTags = new HashMap<>();
            List<String> paragraphBlocks = new ArrayList<>();

            try {
                while ((line = reader.readLine()) != null) {
                    if (line.length() == 0) {
                        if (closingTag != null) {
                            for (Map.Entry<String, Integer> entry : beginOfTags.entrySet()) {
                                paragraphBlocks.set(entry.getValue(), entry.getKey());
                            }
                            for (String block : paragraphBlocks) {
                                writer.write(block);
                            }

                            writer.write(closingTag);
                            writer.newLine();
                            closingTag = null;

                            beginOfTags.clear();
                            paragraphBlocks.clear();
                        }
                        continue;
                    }
                    int beginOfNewBlock = 0;
                    String openingMarkdown = null;

                    if (closingTag == null) {
                        for (String key : md2LineHtml.keySet()) {
                            if (key != null && key.length() <= line.length() && line.substring(0, key.length()).equals(key)) {
                                openingMarkdown = key;
                                beginOfNewBlock = key.length();
                                break;
                            }
                        }
                        closingTag = md2LineHtml.get(openingMarkdown)[1];
                        paragraphBlocks.add(md2LineHtml.get(openingMarkdown)[0]);
                    } else {
                        paragraphBlocks.add(System.lineSeparator());
                    }

                    for (int i = 0; line != null && i < line.length(); i++) {

                        String markdown = null;

                        for (String key : md2InlineHtml.keySet()) {
                            if (i + key.length() <= line.length() && line.substring(i, i + key.length()).equals(key)) {
                                markdown = key;
                                break;
                            }
                        }

                        if (markdown == null) {
                            continue;
                        }

                        paragraphBlocks.add(line.substring(beginOfNewBlock, i));

                        if (md2InlineHtml.get(markdown).length == 3) {
                            StringBuilder link = new StringBuilder();
                            if (beginOfTags.containsKey(pairMd.get(markdown))) {
                                int beginOfData = i, endOfData = -1;
                                do {
                                    for (int j = beginOfData; j < line.length(); j++) {
                                        if (line.charAt(j) == '(') {
                                                beginOfData = j + 1;
                                        } else if (line.charAt(j) == ')') {
                                            endOfData = j;
                                            break;
                                        }
                                    }
                                    if (endOfData == -1) {
                                        link.append(line.substring(beginOfData));
                                        link.append(System.lineSeparator());
                                        beginOfData = 0;
                                    }
                                } while (endOfData == -1 && (line = reader.readLine()) != null);

                                if (line != null) {
                                    link.append(line.substring(beginOfData, endOfData));
                                }

                                paragraphBlocks.set(
                                        beginOfTags.get(pairMd.get(markdown)),
                                        md2InlineHtml.get(markdown)[0] + link.toString() + md2InlineHtml.get(markdown)[1]
                                );

                                beginOfTags.remove(pairMd.get(markdown));
                                paragraphBlocks.add(md2InlineHtml.get(markdown)[2]);
                                i = endOfData;
                                beginOfNewBlock = i + 1;
                            } else {
                                beginOfTags.put(markdown, paragraphBlocks.size());
                                paragraphBlocks.add(markdown);
                                beginOfNewBlock = i + markdown.length();
                                i += markdown.length() - 1;
                            }

                        } else if (md2InlineHtml.get(markdown).length == 2) {
                            if (beginOfTags.containsKey(markdown)) {
                                beginOfTags.remove(markdown);
                                paragraphBlocks.add(md2InlineHtml.get(markdown)[1]);
                            } else {
                                beginOfTags.put(markdown, paragraphBlocks.size());
                                paragraphBlocks.add(md2InlineHtml.get(markdown)[0]);
                            }

                            beginOfNewBlock = i + markdown.length();
                            i += markdown.length() - 1;
                        } else {
                            paragraphBlocks.add(md2InlineHtml.get(markdown)[0]);
                            beginOfNewBlock = i + markdown.length();
                            i += markdown.length() - 1;
                        }

                    }

                    if ( line != null ) {
                        paragraphBlocks.add(line.substring(beginOfNewBlock));
                    }
                }

                if (closingTag != null) {
                    for (Map.Entry<String, Integer> entry : beginOfTags.entrySet()) {
                        paragraphBlocks.set(entry.getValue(), entry.getKey());
                    }

                    for (String block : paragraphBlocks) {
                        writer.write(block);
                    }

                    writer.write(closingTag);
                    writer.newLine();
                }
            } catch (IOException e) {
                System.out.println(e.getMessage());
            } finally {
                reader.close();
                writer.close();
            }


        } catch (IOException e) {
            System.out.println("Error, cannot open file: " + e.getMessage());
        }
    }
}
