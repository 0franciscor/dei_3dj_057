using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class LoadTime : ValueObject
    {
        private float time { get; }
        
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