namespace EletricGo.Domain.Shared {
    public abstract class Entity<TEntityId> where TEntityId : EntityID
    {
        public TEntityId Id { get; protected set; }
    }
}      