using EletricGo.Domain.Shared;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{
    public class Destination : IValueObject
    {
        public string destination { get; }
        
        public Destination() { }

        public Destination(string destination)
        {
            this.destination = destination;
        }

        public string AsString()
        {
            return destination;
        }


        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return destination;
        }
        
    }    
}