//! x86-64 minimal emulator for testing codegen.

/// A rudimentary x86 emulator for a subset of the x86-64 instruction
/// set. The emulator isn't designed for emulating encoded instructions
/// instead it process a raw textual representation of the assembly.
///
/// This kind of emulation is great to ensure the assembly generated from
/// the intermediate representation is consistent and executes correctly.
/// Since our codegen passes will be basic and have no vectorization or
/// baroque instructions we can get away with only emulating the x86 subset
/// we do generate.
///
/// This choice was made mainly because we don't want to write an assembler
/// and linker later on and instead depend on a second toolchain for that.
pub struct Emulator {
    registers: Vec<u64>,
    mmu: Vec<u8>,
}
