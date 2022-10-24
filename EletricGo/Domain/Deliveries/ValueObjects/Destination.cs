using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class Destination : ValueObject
    {
        private string destination { get; }
        

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