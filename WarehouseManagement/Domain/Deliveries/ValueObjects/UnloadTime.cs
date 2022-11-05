using EletricGo.Domain.Shared;
using System.Collections.Generic;
using System;

namespace EletricGo.Domain.Deliveries
{
    public class UnloadTime : IValueObject
    {
        public float time { get; }

        public UnloadTime(){}
        public UnloadTime(float time)
        {
            if(time < 0)
                throw new BusinessRuleValidationException("Unload time cannot be negative.");
            this.time = time;
        }

        public float AsFloat()
        {
            return time;
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            UnloadTime unloadTime = (UnloadTime)obj;
            return (time == unloadTime.time);
        }

        override
        public int GetHashCode()
        {
            return time.GetHashCode();
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return time;
        }
    }
}