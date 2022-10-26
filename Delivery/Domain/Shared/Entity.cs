namespace EletricGo.Domain.Shared {
    public abstract class Entity<TEntityId> where TEntityId : EntityID
    {
        public string Id { get; protected set; }
    }
}      