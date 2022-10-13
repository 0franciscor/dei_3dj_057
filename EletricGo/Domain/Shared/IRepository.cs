namespace EletricGo.Domain.Shared
{
    public interface IRepository<TEntity,TEntityId>
    {
        Task<TEntity> Get(TEntityId id);
        Task<List<TEntity>> GetAll();
        Task Add(TEntity entity);
        Task Update(TEntity entity);
        Task Delete(TEntity entity);
    }
}