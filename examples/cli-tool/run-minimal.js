import { main } from './minimal-test-new.js';

(async () => {
    try {
        const result = await main();
        console.log("Result:", result);
        if (result && result.type === 'Err') {
            console.error("Error:", result.value);
            process.exit(1);
        }
    } catch (error) {
        console.error("Unhandled error:", error);
        process.exit(1);
    }
})();