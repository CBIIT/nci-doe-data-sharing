package gov.nih.nci.doe.web.util;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The type Lambda utils.
 */
public class LambdaUtils {

    /**
     * This is a very simple utility function that takes a List of type T and a function
     * (provided by the caller), and applies that function to every element of the source
     * List, returning a List of the results of that transformation.
     * <p>
     * For example, if list is a collection of database objects, f could extract their ID
     * fields into a new list and return it.
     *
     * @param <T>  the type parameter
     * @param <R>  the type parameter
     * @param list the list
     * @param f    the f
     * @return list
     */
    public static <T, R> List<R> map(List<T> list, Function<T, R> f) {
        List<R> result = new ArrayList<>(list.size());

        for(T s: list) {
            result.add(f.apply(s));
        }

        return result;
    }

    /**
     * Filter list.
     *
     * @param <T>  the type parameter
     * @param list the list
     * @param p    the p
     * @return list
     */
    public static <T> List<T> filter(List<T> list, Predicate<T> p) {
        List<T> results = new ArrayList<>(list.size());

        for (T t: list) {
            if(p.test(t)) {
                results.add(t);
            }

        }

        return results;
    }
}
