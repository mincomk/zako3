import { Outlet } from 'react-router-dom'

export const RootLayout = () => {
  return (
    <div className="min-h-svh bg-background text-foreground antialiased">
      <Outlet />
    </div>
  )
}
