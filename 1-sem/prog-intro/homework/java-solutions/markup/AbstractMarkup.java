package markup;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractMarkup implements ParagraphElement{
    private final List<ParagraphElement> elements;

    public AbstractMarkup(List<ParagraphElement> elements) {
        this.elements = new ArrayList<>(elements);
    }

    protected void toMarkdown(StringBuilder result, String begin, String end) {
        result.append(begin);
        for (ParagraphElement element : elements) {
            element.toMarkdown(result);
        }
        result.append(end);
    }

    protected void toTex(StringBuilder result, String begin, String end) {
        result.append(begin);
        for (ParagraphElement element : elements) {
            element.toTex(result);
        }
        result.append(end);
    }
}
