using EletricGo.Domain.Shared;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace EletricGo.Infrastructure.Shared
{
    public class BaseRepository<TEntity,TEntityId> : IRepository<TEntity,TEntityId>
    where TEntity : Entity<TEntityId>
    where TEntityId : EntityID
    {
        private readonly DbSet<TEntity> _objs;

        public BaseRepository(DbSet<TEntity> objs)
        {
            this._objs = objs ?? throw new ArgumentNullException(nameof(objs));

        }
        public async Task<TEntity> GetByID(TEntityId id)
        {
            return await this._objs.FindAsync(id.Value);
            /*return await this._objs.
                Where(x => id.Equals(x.Id)).FirstOrDefaultAsync();*/
        }
        
        public async Task<List<TEntity>> GetAll()
        {
            return await this._objs.ToListAsync();
        }

        public async Task<TEntity> Add(TEntity obj)
        {
            var ret = await this._objs.AddAsync(obj);
            return ret.Entity;
        }

        public void Delete(TEntity obj)
        {
            this._objs.Remove(obj);
        }
    }
}