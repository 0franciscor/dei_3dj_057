using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Autonomy : IValueObject<Autonomy>
    {

        private float autonomy;

        public Autonomy(float autonomy)
        {
            this.autonomy = autonomy;
        }

        public String toString()
        {
            return autonomy.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

        public float AsFloat()
        {
            return autonomy;
        }

    }    
}