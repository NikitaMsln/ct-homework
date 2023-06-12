package markup;

import java.util.List;

public class OrderedList extends AbstractTexList{
    public OrderedList(List<ListItem> items) {
        super(items);
    }

    @Override
    public void toTex(StringBuilder result) {
        super.toTex(result, "\\begin{enumerate}", "\\end{enumerate}");
    }
}
