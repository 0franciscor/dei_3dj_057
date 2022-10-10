namespace EletricGo.Domain.Shared {
    interface ValueObject
    {
        String toString();

        Boolean equals(Object obj);

        int hashCode();
    }
}