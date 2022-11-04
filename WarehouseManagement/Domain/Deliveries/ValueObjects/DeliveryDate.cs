using EletricGo.Domain.Shared;
using System;
using System.Collections.Generic;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDate : IValueObject
    {
        public DateTime date { get; }

        public DeliveryDate() { }
        
        public DeliveryDate(DateTime date)
        {
            if (date == default(DateTime))
                throw new ArgumentNullException("Date cannot be null");
            if (date < DateTime.Now)
                throw new BusinessRuleValidationException("Date cannot be in the past");

            this.date = date;
        }

        public DateTime AsDateTime()
        {
            return date;
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            DeliveryDate deliveryDate = (DeliveryDate)obj;
            return (date == deliveryDate.date);
        }

        override
        public int GetHashCode()
        {
            return date.GetHashCode();
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return date;
        }
    }
   
}