package markup;

import java.util.List;

public class Emphasis extends AbstractMarkup {
    public Emphasis(List<ParagraphElement> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        super.toMarkdown(result, "*", "*");
    }

    @Override
    public void toTex(StringBuilder result) {
        super.toTex(result, "\\emph{", "}");
    }
}
