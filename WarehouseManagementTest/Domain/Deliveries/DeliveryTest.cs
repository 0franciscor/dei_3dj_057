using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;

namespace WarehouseManagementTest.Domain.Deliveries
{
    [TestFixture]
    internal class DeliveryTest
    {
        [Test]
        public void CreateDeliveryDateInsuccess()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new DeliveryDate(new DateTime(2020, 12, 12)));
            Assert.That(ex.Message, Is.EqualTo("Date cannot be in the past."));
        }


        [Test]
        public void CreateDeliveryDateSuccess()
        {
            var datetime = new DateTime(2023, 12, 12);
            var deliveryDate = new DeliveryDate(datetime);

            Assert.That(datetime, Is.EqualTo(deliveryDate.AsDateTime()));
        }

        [Test]
        public void CreateDeliveryMassInsuccess()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new DeliveryMass(-3));
            Assert.That(ex.Message, Is.EqualTo("Mass must be greater than 0"));
        }

        [Test]
        public void CreateDeliveryMassSuccess()
        {
            float mass = 15f;
            var deliveryMass = new DeliveryMass(mass);

            Assert.That(mass, Is.EqualTo(deliveryMass.AsFloat()));
        }
    }
}
