package markup;

import java.util.List;

public class Strikeout extends AbstractMarkup {
    public Strikeout(List<ParagraphElement> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        super.toMarkdown(result, "~", "~");
    }

    @Override
    public void toTex(StringBuilder result) {
        super.toTex(result, "\\textst{", "}");
    }
}
