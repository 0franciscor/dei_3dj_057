using EletricGo.Domain.Shared;
using System;
using System.Collections.Generic;

namespace EletricGo.Domain.Deliveries
{
    public class Destination : IValueObject
    {
        public string destination { get; }
        
        public Destination() { }

        public Destination(string destination)
        {
            if (destination == null)
                throw new BusinessRuleValidationException("Destination cannot be null.");
            if (destination == "")
                throw new BusinessRuleValidationException("Destination cannot be empty.");
            this.destination = destination;
        }

        public string AsString()
        {
            return destination;
        }

        override
        public int GetHashCode()
        {
            return destination.GetHashCode();
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            Destination destination = (Destination)obj;
            return this.destination == destination.destination;
        }


        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return destination;
        }
        
    }    
}