using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses.ValueObjects;

namespace EletricGo.Domain.Warehouses;

public interface IWarehouseRepository : IRepository<Warehouse, WarehouseID>
{
    
}