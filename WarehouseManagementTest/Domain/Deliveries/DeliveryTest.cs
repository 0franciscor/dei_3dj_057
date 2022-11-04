using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;

namespace WarehouseManagementTest.Domain.Deliveries
{
    [TestFixture]
    internal class DeliveryTest
    {
        [Test]
        public void CreateDeliveryDateInSuccess()
        {
            var ex = Assert.Throws<BusinessRuleValidationException>(() => new DeliveryDate(new DateTime(2020, 12, 12)));
            Assert.That(ex.Message, Is.EqualTo("Date cannot be in the past."));
        }


        [Test]
        public void CreateDeliveryDateInuccess()
        {
            var datetime = new DateTime(2023, 12, 12);
            var deliveryDate = new DeliveryDate(datetime);

            Assert.That(datetime, Is.EqualTo(deliveryDate.AsDateTime()));
        }
    }
}
