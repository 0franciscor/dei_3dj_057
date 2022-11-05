using EletricGo.Domain.Shared;
using System.Collections.Generic;
using System;

namespace EletricGo.Domain.Deliveries
{
    public class LoadTime : IValueObject
    {
        public float time { get; }

        public LoadTime() { }
        
        public LoadTime(float time)
        {
            if(time < 0)
                throw new BusinessRuleValidationException("Load time cannot be negative.");
            this.time = time;
        }

        public float AsFloat()
        {
            return time;
        }

        override
        public int GetHashCode()
        {
            return time.GetHashCode();
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            LoadTime loadTime = (LoadTime)obj;
            return time == loadTime.time;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return time;
        }
    }
   
  
}