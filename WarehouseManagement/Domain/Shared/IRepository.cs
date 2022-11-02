using System.Collections.Generic;
using System.Threading.Tasks;

namespace EletricGo.Domain.Shared
{
    public interface IRepository<TEntity, TEntityId>
    {
        Task<TEntity> GetByID(TEntityId id);

        Task<List<TEntity>> GetAll();
        
        //Task<List<TEntity>> GetByDescription(string description);

        Task<TEntity> Add(TEntity entity);

        void Delete(TEntity entity);

        Task<bool> Find(TEntityId id);
    }
}