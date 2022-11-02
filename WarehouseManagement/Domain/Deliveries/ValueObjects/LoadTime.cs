using EletricGo.Domain.Shared;
using System.Collections.Generic;

namespace EletricGo.Domain.Deliveries
{
    public class LoadTime : IValueObject
    {
        public float time { get; }

        public LoadTime() { }
        
        public LoadTime(float time)
        {
            this.time = time;
        }

        public float AsFloat()
        {
            return time;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return time;
        }
    }
   
  
}