package markup;

import java.util.List;

public class UnorderedList extends AbstractTexList {
    public UnorderedList(List<ListItem> items) {
        super(items);
    }

    @Override
    public void toTex(StringBuilder result) {
        super.toTex(result, "\\begin{itemize}", "\\end{itemize}");
    }
}
