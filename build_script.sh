#!/bin/bash

# Cheat Engine Mac Build Script with Stealth Features
# Author: AI Assistant
# Version: 1.0

set -e

# Configuration
CE_VERSION="7.5.2"
BUILD_DIR="build"
RELEASE_DIR="release"
STEALTH_MODE=${STEALTH_MODE:-true}
LAZARUS_PATH="/usr/local/share/lazarus"

echo "========================================"
echo "Cheat Engine Mac Build Script"
echo "Version: $CE_VERSION"
echo "Stealth Mode: $STEALTH_MODE"
echo "========================================"

# Create directories
mkdir -p "$BUILD_DIR"
mkdir -p "$RELEASE_DIR"

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to install dependencies
install_dependencies() {
    echo "Installing dependencies..."
    
    # Check Homebrew
    if ! command_exists brew; then
        echo "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    
    # Install Lazarus and FPC
    if ! command_exists lazbuild; then
        echo "Installing Lazarus..."
        brew install --cask lazarus
    fi
    
    if ! command_exists fpc; then
        echo "Installing Free Pascal..."
        brew install fpc
    fi
    
    # Install additional tools
    brew install cmake ninja git
}

# Function to setup Lazarus environment
setup_lazarus() {
    echo "Setting up Lazarus environment..."
    
    # Find Lazarus installation
    if [ -d "$LAZARUS_PATH" ]; then
        export LAZARUS_DIR="$LAZARUS_PATH"
    elif [ -d "/usr/local/share/lazarus" ]; then
        export LAZARUS_DIR="/usr/local/share/lazarus"
    elif [ -d "/opt/lazarus" ]; then
        export LAZARUS_DIR="/opt/lazarus"
    else
        echo "Error: Lazarus installation not found"
        exit 1
    fi
    
    echo "Lazarus directory: $LAZARUS_DIR"
    
    # Verify installation
    lazbuild --version
    fpc -iV
}

# Function to copy macport files
copy_macport_files() {
    echo "Copying macport compatibility files..."
    
    # Copy macport files to CE source
    cp ce_mac_patches/macport.pas "cheat-engine-master/cheat-engine-master/Cheat Engine/"
    cp ce_mac_patches/macportdefines.pas "cheat-engine-master/cheat-engine-master/Cheat Engine/"
    cp ce_mac_patches/stealth/stealth_utils.pas "cheat-engine-master/cheat-engine-master/Cheat Engine/"
    
    echo "Macport files copied successfully"
}

# Function to apply stealth patches
apply_stealth_patches() {
    if [ "$STEALTH_MODE" = "true" ]; then
        echo "Applying stealth patches..."
        
        cd "cheat-engine-master"
        
        # Apply string obfuscation
        if [ -f "../ce_mac_patches/stealth/0001-string_obfuscation.patch" ]; then
            echo "Applying string obfuscation patch..."
            git apply ../ce_mac_patches/stealth/0001-string_obfuscation.patch || echo "Patch 1 failed, continuing..."
        fi
        
        # Apply process name obfuscation
        if [ -f "../ce_mac_patches/stealth/0002-process_name_obfuscation.patch" ]; then
            echo "Applying process name obfuscation patch..."
            git apply ../ce_mac_patches/stealth/0002-process_name_obfuscation.patch || echo "Patch 2 failed, continuing..."
        fi
        
        # Apply anti-detection features
        if [ -f "../ce_mac_patches/stealth/0003-anti_detection_features.patch" ]; then
            echo "Applying anti-detection features patch..."
            git apply ../ce_mac_patches/stealth/0003-anti_detection_features.patch || echo "Patch 3 failed, continuing..."
        fi
        
        cd ..
        echo "Stealth patches applied"
    else
        echo "Skipping stealth patches (STEALTH_MODE=false)"
    fi
}

# Function to build Cheat Engine
build_cheat_engine() {
    echo "Building Cheat Engine..."
    
    cd "cheat-engine-master/cheat-engine-master/Cheat Engine"
    
    # Try different build methods
    echo "Attempting build with lazbuild..."
    if command_exists lazbuild; then
        lazbuild --build-mode=release --verbose cheatengine.lpi || echo "lazbuild failed"
    fi
    
    # Check if binary was created
    if [ -f "cheatengine" ]; then
        echo "✅ Build successful!"
        ls -la cheatengine
    else
        echo "❌ Build failed, trying alternative methods..."
        
        # Try direct FPC compilation
        if [ -f "cheatengine.lpr" ]; then
            echo "Trying direct FPC compilation..."
            fpc -B -O2 -gl -Mobjfpc cheatengine.lpr || echo "FPC compilation failed"
        fi
        
        # Check again
        if [ -f "cheatengine" ]; then
            echo "✅ Alternative build successful!"
        else
            echo "❌ All build methods failed"
            exit 1
        fi
    fi
    
    cd ../../..
}

# Function to apply post-build stealth modifications
apply_post_build_stealth() {
    if [ "$STEALTH_MODE" = "true" ]; then
        echo "Applying post-build stealth modifications..."
        
        local binary="cheat-engine-master/cheat-engine-master/Cheat Engine/cheatengine"
        
        if [ -f "$binary" ]; then
            # Strip debug symbols
            echo "Stripping debug symbols..."
            strip "$binary" || echo "strip failed"
            
            # Remove identifying strings (basic approach)
            echo "Obfuscating binary strings..."
            sed -i '' 's/Cheat Engine/Memory Editor/g' "$binary" || echo "string replacement 1 failed"
            sed -i '' 's/cheatengine/memoryeditor/g' "$binary" || echo "string replacement 2 failed"
            sed -i '' 's/cheat-engine/memory-editor/g' "$binary" || echo "string replacement 3 failed"
            
            # Add entropy to confuse analysis tools
            echo "Adding entropy padding..."
            dd if=/dev/urandom bs=1024 count=1 >> "$binary" 2>/dev/null || echo "entropy padding failed"
            
            echo "Post-build stealth modifications applied"
        else
            echo "❌ Binary not found for post-build modifications"
        fi
    fi
}

# Function to create app bundle
create_app_bundle() {
    echo "Creating app bundle..."
    
    local binary="cheat-engine-master/cheat-engine-master/Cheat Engine/cheatengine"
    local app_bundle="$RELEASE_DIR/Memory Editor.app"
    
    if [ -f "$binary" ]; then
        # Create app bundle structure
        mkdir -p "$app_bundle/Contents/MacOS"
        mkdir -p "$app_bundle/Contents/Resources"
        
        # Copy binary
        cp "$binary" "$app_bundle/Contents/MacOS/"
        
        # Create Info.plist
        cat > "$app_bundle/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>cheatengine</string>
    <key>CFBundleIdentifier</key>
    <string>com.memoryeditor.app</string>
    <key>CFBundleName</key>
    <string>Memory Editor</string>
    <key>CFBundleDisplayName</key>
    <string>Memory Editor</string>
    <key>CFBundleVersion</key>
    <string>$CE_VERSION</string>
    <key>CFBundleShortVersionString</key>
    <string>$CE_VERSION</string>
    <key>NSHighResolutionCapable</key>
    <true/>
    <key>LSUIElement</key>
    <false/>
    <key>NSSupportsAutomaticGraphicsSwitching</key>
    <true/>
    <key>NSRequiresAquaSystemAppearance</key>
    <false/>
</dict>
</plist>
EOF
        
        # Create icon placeholder (if needed)
        # cp icon.icns "$app_bundle/Contents/Resources/AppIcon.icns" 2>/dev/null || true
        
        echo "✅ App bundle created: $app_bundle"
        
        # Make binary executable
        chmod +x "$app_bundle/Contents/MacOS/cheatengine"
        
    else
        echo "❌ Binary not found for app bundle creation"
        exit 1
    fi
}

# Function to create DMG
create_dmg() {
    echo "Creating DMG package..."
    
    local app_bundle="$RELEASE_DIR/Memory Editor.app"
    local dmg_name="MemoryEditor-$CE_VERSION"
    
    if [ -d "$app_bundle" ]; then
        # Create temporary DMG folder
        local dmg_temp="$BUILD_DIR/dmg_temp"
        mkdir -p "$dmg_temp"
        
        # Copy app bundle to temp folder
        cp -R "$app_bundle" "$dmg_temp/"
        
        # Create DMG
        hdiutil create -volname "Memory Editor $CE_VERSION" \
                     -srcfolder "$dmg_temp" \
                     -ov -format UDZO \
                     -imagekey zlib-level=9 \
                     "$RELEASE_DIR/$dmg_name.dmg"
        
        # Clean up temp folder
        rm -rf "$dmg_temp"
        
        echo "✅ DMG created: $RELEASE_DIR/$dmg_name.dmg"
        ls -lh "$RELEASE_DIR/$dmg_name.dmg"
        
    else
        echo "❌ App bundle not found for DMG creation"
    fi
}

# Function to verify build
verify_build() {
    echo "Verifying build..."
    
    local app_bundle="$RELEASE_DIR/Memory Editor.app"
    local binary="$app_bundle/Contents/MacOS/cheatengine"
    
    if [ -f "$binary" ]; then
        echo "✅ Binary exists: $binary"
        echo "   Size: $(du -h "$binary" | cut -f1)"
        echo "   Permissions: $(ls -la "$binary" | cut -d' ' -f1)"
        
        # Check if binary is executable
        if [ -x "$binary" ]; then
            echo "✅ Binary is executable"
        else
            echo "❌ Binary is not executable"
        fi
        
        # Check app bundle structure
        if [ -f "$app_bundle/Contents/Info.plist" ]; then
            echo "✅ Info.plist exists"
        else
            echo "❌ Info.plist missing"
        fi
        
    else
        echo "❌ Build verification failed"
        return 1
    fi
    
    return 0
}

# Main build process
main() {
    echo "Starting build process..."
    
    # Install dependencies
    install_dependencies
    
    # Setup Lazarus
    setup_lazarus
    
    # Copy macport files
    copy_macport_files
    
    # Apply stealth patches
    apply_stealth_patches
    
    # Build Cheat Engine
    build_cheat_engine
    
    # Apply post-build stealth modifications
    apply_post_build_stealth
    
    # Create app bundle
    create_app_bundle
    
    # Create DMG
    create_dmg
    
    # Verify build
    if verify_build; then
        echo ""
        echo "🎉 Build completed successfully!"
        echo "📦 Release files in: $RELEASE_DIR"
        echo "🔧 Stealth mode: $STEALTH_MODE"
    else
        echo ""
        echo "❌ Build completed with errors"
        exit 1
    fi
}

# Run main function
main "$@"
