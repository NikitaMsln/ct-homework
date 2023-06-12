package markup;

import java.util.List;

public class Strong extends AbstractMarkup{
    public Strong(List<ParagraphElement> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        super.toMarkdown(result, "__", "__");
    }

    @Override
    public void toTex(StringBuilder result) {
        super.toTex(result, "\\textbf{", "}");
    }
}
