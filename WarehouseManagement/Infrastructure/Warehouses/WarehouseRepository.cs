using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.ValueObjects;
using EletricGo.Infrastructure.Shared;

namespace EletricGo.Infrastructure.Warehouses;

public class WarehouseRepository : BaseRepository<Warehouse, WarehouseID>, IWarehouseRepository
{
    public WarehouseRepository(EletricGoDBContext context) : base(context.Warehouse)
    {
    }
}