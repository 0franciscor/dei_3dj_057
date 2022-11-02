using EletricGo.Domain.Shared;
using System.Collections.Generic;

namespace EletricGo.Domain.Deliveries
{
    public class UnloadTime : IValueObject
    {
        public float time { get; }

        public UnloadTime(){}
        public UnloadTime(float time)
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