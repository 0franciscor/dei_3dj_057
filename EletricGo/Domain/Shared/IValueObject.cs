namespace EletricGo.Domain.Shared {
    
    interface IValueObject<T>
    {
        String toString();

        Boolean equals(Object obj);

        int hashCode();
    }
}