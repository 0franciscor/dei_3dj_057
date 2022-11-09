using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;

namespace WarehouseManagementTest.Unit.Domain
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
            Assert.That(ex.Message, Is.EqualTo("Delivery's Mass must be greater than 0."));
        }

        [Test]
        public void CreateDeliveryMassSuccess()
        {
            float mass = 15f;
            var deliveryMass = new DeliveryMass(mass);

            Assert.That(mass, Is.EqualTo(deliveryMass.AsFloat()));
        }

        [Test]
        public void CreateDestinationInsuccessNull()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new Destination(null));
            Assert.That(ex.Message, Is.EqualTo("Destination cannot be null."));
        }

        [Test]
        public void CreateDestinationInsuccessEmpty()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new Destination(""));
            Assert.That(ex.Message, Is.EqualTo("Destination cannot be empty."));
        }

        [Test]
        public void CreateDestinationSuccess()
        {
            string destination = "Rua do ISEP";
            var deliveryDestination = new Destination(destination);

            Assert.That(destination, Is.EqualTo(deliveryDestination.AsString()));
        }

        [Test]
        public void CreateUnloadTimeInsuccess()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new UnloadTime(-3));
            Assert.That(ex.Message, Is.EqualTo("Unload time cannot be negative."));
        }

        [Test]
        public void CreateUnloadTimeSuccess()
        {
            float time = 15f;
            var unloadTime = new UnloadTime(time);

            Assert.That(time, Is.EqualTo(unloadTime.AsFloat()));
        }

        [Test]
        public void CreateLoadTimeInsuccess()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new LoadTime(-3));
            Assert.That(ex.Message, Is.EqualTo("Load time cannot be negative."));
        }

        [Test]
        public void CreateLoadTimeSuccess()
        {
            float time = 15f;
            var loadTime = new LoadTime(time);

            Assert.That(time, Is.EqualTo(loadTime.AsFloat()));
        }
    }
}
