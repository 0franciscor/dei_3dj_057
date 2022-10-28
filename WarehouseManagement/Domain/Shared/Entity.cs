namespace EletricGo.Domain.Shared {

    public abstract class Entity<TEntityId>
    {
        public string Id { get; protected set; }
    }
}      