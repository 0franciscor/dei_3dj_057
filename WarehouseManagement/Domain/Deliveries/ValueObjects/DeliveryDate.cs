using EletricGo.Domain.Shared;
using System;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDate : IValueObject
    {
        public DateTime date { get; }

        public DeliveryDate() { }
        
        public DeliveryDate(DateTime date)
        {
            this.date = date;
        }

        public DateTime AsDateTime()
        {
            return date;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return date;
        }
    }
   
}