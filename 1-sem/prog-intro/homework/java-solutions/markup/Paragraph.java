package markup;

import java.util.ArrayList;
import java.util.List;

public class Paragraph implements MarkdownElement, TexItem {
    private final List<ParagraphElement> elements;

    public Paragraph(List<ParagraphElement> elements) {
        this.elements = new ArrayList<>(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        for (ParagraphElement element : elements) {
            element.toMarkdown(result);
        }
    }

    @Override
    public void toTex(StringBuilder result) {
        for (ParagraphElement element : elements) {
            element.toTex(result);
        }
    }
}
