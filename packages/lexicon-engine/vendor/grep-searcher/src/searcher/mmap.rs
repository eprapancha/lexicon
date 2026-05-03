#[cfg(feature = "mmap")]
mod inner {
    use std::{fs::File, path::Path};

    use memmap::Mmap;

    /// Controls the strategy used for determining when to use memory maps.
    ///
    /// If a searcher is called in circumstances where it is possible to use memory
    /// maps, and memory maps are enabled, then it will attempt to do so if it
    /// believes it will make the search faster.
    ///
    /// By default, memory maps are disabled.
    #[derive(Clone, Debug)]
    pub struct MmapChoice(MmapChoiceImpl);

    #[derive(Clone, Debug)]
    enum MmapChoiceImpl {
        Auto,
        Never,
    }

    impl Default for MmapChoice {
        fn default() -> MmapChoice {
            MmapChoice(MmapChoiceImpl::Never)
        }
    }

    impl MmapChoice {
        /// Use memory maps when they are believed to be advantageous.
        ///
        /// # Safety
        ///
        /// The caller guarantees that the underlying file won't be mutated.
        pub unsafe fn auto() -> MmapChoice {
            MmapChoice(MmapChoiceImpl::Auto)
        }

        /// Never use memory maps, no matter what. This is the default.
        pub fn never() -> MmapChoice {
            MmapChoice(MmapChoiceImpl::Never)
        }

        /// Return a memory map if memory maps are enabled and if creating a
        /// memory from the given file succeeded and if memory maps are believed
        /// to be advantageous for performance.
        pub(crate) fn open(
            &self,
            file: &File,
            path: Option<&Path>,
        ) -> Option<Mmap> {
            if !self.is_enabled() {
                return None;
            }
            if cfg!(target_os = "macos") {
                return None;
            }
            match unsafe { Mmap::map(file) } {
                Ok(mmap) => Some(mmap),
                Err(err) => {
                    if let Some(path) = path {
                        log::debug!(
                            "{}: failed to open memory map: {}",
                            path.display(),
                            err
                        );
                    } else {
                        log::debug!("failed to open memory map: {}", err);
                    }
                    None
                }
            }
        }

        /// Whether this strategy may employ memory maps or not.
        pub(crate) fn is_enabled(&self) -> bool {
            match self.0 {
                MmapChoiceImpl::Auto => true,
                MmapChoiceImpl::Never => false,
            }
        }
    }
}

#[cfg(not(feature = "mmap"))]
mod inner {
    /// Controls the strategy used for determining when to use memory maps.
    ///
    /// When the `mmap` feature is disabled (e.g., for WASM builds), memory maps
    /// are never available and this type is a stub that always returns "never".
    #[derive(Clone, Debug)]
    pub struct MmapChoice(());

    impl Default for MmapChoice {
        fn default() -> MmapChoice {
            MmapChoice(())
        }
    }

    impl MmapChoice {
        /// Never use memory maps. This is the only option when mmap is disabled.
        pub fn never() -> MmapChoice {
            MmapChoice(())
        }

        /// Whether this strategy may employ memory maps or not.
        /// Always returns false when the mmap feature is disabled.
        pub(crate) fn is_enabled(&self) -> bool {
            false
        }
    }
}

pub use self::inner::MmapChoice;
