using EletricGo.Domain.Deliveries;
using EletricGo.Infrastructure.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;
using System;
using System.Linq;
using Microsoft.EntityFrameworkCore;

namespace EletricGo.Infrastructure.Deliveries
{
    public class DeliveryRepository : BaseRepository<Delivery, DeliveryID>, IDeliveryRepository
    {
        public DeliveryRepository(EletricGoDBContext context) : base(context.Delivery)
        {

        }

        public async Task<List<Delivery>> GetByPeriod(DateTime date1, DateTime date2)
        {
            return await this._objs.Where(x => x.deliveryDate.date >= date1 && x.deliveryDate.date <= date2).ToListAsync();
        }
    }

}