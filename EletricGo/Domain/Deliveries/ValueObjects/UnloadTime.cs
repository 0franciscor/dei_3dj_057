using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class UnloadTime : ValueObject
    {
        private float Time { get; }

        public UnloadTime(float time)
        {
            Time = time;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Time;
        }
    }
}