using EletricGo.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;
using System;

namespace EletricGo.Domain.Deliveries
{ 
    public interface IDeliveryRepository : IRepository<Delivery, DeliveryID>
    {
        Task<List<Delivery>> GetByPeriod(DateTime date1, DateTime date2);
    }

}