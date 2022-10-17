using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class LoadTime : ValueObject
    {
        private float Time { get; }

        public LoadTime(float time)
        {
            Time = time;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Time;
        }
    }
   
  
}